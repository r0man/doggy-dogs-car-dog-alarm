import 'dart:async';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import '../models/alarm_state.dart';
import '../models/sensor_data.dart';
import '../models/dog.dart';
import '../providers/dog_provider.dart';
import 'sensor_detection_service.dart';
import 'bark_audio_service.dart';
import 'background_monitoring_service.dart';
import 'notification_service.dart';
import 'alarm_persistence_service.dart';
import 'unlock_code_service.dart';
import 'alarm_state_manager.dart';
import 'alarm_timer.dart';
import 'alarm_conditions.dart';

/// Service for managing the car alarm system
/// This is now a thin orchestration layer over testable business logic
class AlarmService {
  final SensorDetectionService sensorService;
  final BarkAudioService barkService;
  final BackgroundMonitoringService backgroundService;
  final NotificationService notificationService;
  final AlarmPersistenceService persistenceService;
  final UnlockCodeService unlockCodeService;
  final int countdownDuration;
  final Dog guardDog;

  // Extracted business logic components (testable)
  final AlarmStateManager _stateManager;
  final AlarmTimer _timer;
  final AlarmConditions _conditions;

  StreamSubscription<MotionEvent>? _motionSubscription;

  // Threat verification
  final List<MotionEvent> _recentMotions = [];
  Timer? _verificationTimer;

  AlarmService({
    required this.sensorService,
    required this.barkService,
    required this.backgroundService,
    required this.notificationService,
    required this.persistenceService,
    required this.unlockCodeService,
    required this.countdownDuration,
    required this.guardDog,
    AlarmStateManager? stateManager,
    AlarmTimer? timer,
    AlarmConditions? conditions,
  })  : _stateManager = stateManager ?? AlarmStateManager(),
        _timer = timer ?? AlarmTimer(),
        _conditions = conditions ??
            AlarmConditions(
              sensitivity: sensorService.sensitivity,
              guardDog: guardDog,
            );

  /// Stream of alarm state changes
  Stream<AlarmState> get alarmStateStream => _stateManager.stateStream;

  /// Current alarm state
  AlarmState get currentState => _stateManager.currentState;

  /// Start activation countdown
  Future<void> startActivation({AlarmMode mode = AlarmMode.standard}) async {
    if (!_stateManager.canStartActivation()) return;

    // Start countdown state
    final state = _stateManager.startCountdown(mode, countdownDuration);

    // Persist countdown state
    await persistenceService.saveAlarmState(state);

    // Start countdown timer
    _timer.start(
      duration: countdownDuration,
      onTick: (remainingSeconds) {
        _stateManager.updateCountdown(remainingSeconds);
        // State is already broadcast by state manager, no need to persist on each tick
      },
      onComplete: () => _completeActivation(),
    );
  }

  /// Complete activation after countdown
  Future<void> _completeActivation() async {
    if (!_stateManager.canCompleteCountdown()) return;

    final mode = _stateManager.currentState.mode;

    // Update state to active
    final state = _stateManager.completeActivation(mode);

    // Persist state
    await persistenceService.saveAlarmState(state);

    // Start monitoring sensors
    await sensorService.startMonitoring();

    // Start background monitoring
    await backgroundService.startMonitoring(mode: mode);

    // Show activation notification
    await notificationService.showAlarmActivated(
      mode: mode,
      dogName: guardDog.name,
    );

    // Subscribe to motion events
    _motionSubscription = sensorService.motionEvents.listen(
      _handleMotionEvent,
      onError: (error) {
        // Propagate error through state manager's stream
        // _stateManager doesn't have addError, so we'll just handle locally
      },
    );
  }

  /// Cancel activation countdown
  Future<void> cancelCountdown() async {
    if (!_stateManager.canCancelCountdown()) return;

    // Cancel timer
    _timer.stop();

    // Update state
    _stateManager.cancelCountdown();

    // Clear persisted state
    await persistenceService.clearAlarmState();
  }

  /// Deactivate the alarm with unlock code validation
  Future<bool> deactivateWithUnlockCode(String unlockCode) async {
    if (!_stateManager.canDeactivate()) return false;

    // Validate unlock code
    final isValid = await unlockCodeService.validateUnlockCode(unlockCode);
    if (!isValid) {
      return false;
    }

    await _deactivate();
    return true;
  }

  /// Internal deactivation logic
  Future<void> _deactivate() async {
    // Stop barking if active
    await barkService.stopBarking();

    // Update state
    _stateManager.deactivate();

    // Clear persisted state
    await persistenceService.clearAlarmState();

    // Stop monitoring sensors
    await _motionSubscription?.cancel();
    await sensorService.stopMonitoring();

    // Stop background monitoring
    await backgroundService.stopMonitoring();

    // Show deactivation notification
    await notificationService.showAlarmDeactivated(dogName: guardDog.name);

    // Clear verification data
    _recentMotions.clear();
    _verificationTimer?.cancel();
  }

  /// Acknowledge and silence a triggered alarm
  Future<void> acknowledge() async {
    if (!_stateManager.canAcknowledge()) return;

    // Stop barking
    await barkService.stopBarking();

    _stateManager.acknowledge();

    // Clear recent motions after acknowledgment
    _recentMotions.clear();
  }

  /// Handle motion event from sensors
  void _handleMotionEvent(MotionEvent event) {
    if (!_stateManager.currentState.isActive) return;

    // Use AlarmConditions to evaluate the motion
    final decision = _conditions.evaluateMotion(
      event,
      _stateManager.currentState.mode,
    );

    if (decision.shouldTrigger) {
      _triggerAlarm(event);
    } else if (decision.reason == TriggerReason.needsVerification) {
      _verifyThreat(event);
    }
    // Otherwise, ignore the motion
  }

  /// Verify if motion is a real threat
  void _verifyThreat(MotionEvent event) {
    // Add to recent motions
    _recentMotions.add(event);

    // Filter out old motions using AlarmConditions
    final filteredMotions = _conditions.filterRecentMotions(_recentMotions);
    _recentMotions.clear();
    _recentMotions.addAll(filteredMotions);

    // Check if threat is verified using AlarmConditions
    if (_conditions.isVerifiedThreat(_recentMotions)) {
      _triggerAlarm(event);
      _recentMotions.clear();
      return;
    }

    // Start verification timer if not already running
    _verificationTimer?.cancel();
    _verificationTimer = Timer(AlarmConditions.verificationWindow, () {
      // If we didn't get enough confirmations, clear the buffer
      _recentMotions.clear();
    });
  }

  /// Trigger the alarm
  void _triggerAlarm(MotionEvent event) {
    if (!_stateManager.canTrigger()) return; // Already triggered

    final state = _stateManager.trigger();

    // Persist triggered state
    persistenceService.saveAlarmState(state);

    // Start barking with current mode
    barkService.startBarking(state.mode);

    // Send notification
    notificationService.showAlarmTriggered(
      motionType: event.type,
      intensity: event.intensity,
      dogName: guardDog.name,
    );

    // TODO: Log event to database for history
  }

  /// Recalibrate sensors (e.g., after car parks in new location)
  Future<void> recalibrate() async {
    await sensorService.recalibrate();
    _recentMotions.clear();
  }

  /// Dispose of resources
  void dispose() {
    _motionSubscription?.cancel();
    _verificationTimer?.cancel();
    _timer.dispose();
    _stateManager.dispose();
  }
}

/// Provider for the alarm service
final alarmServiceProvider = Provider<AlarmService>((ref) {
  final sensitivity = ref.watch(alarmSensitivityProvider);
  final sensorService = ref.watch(
    sensorDetectionServiceProvider(sensitivity),
  );
  final backgroundService = ref.watch(backgroundMonitoringServiceProvider);
  final notificationService = ref.watch(notificationServiceProvider);
  final persistenceService = ref.watch(alarmPersistenceServiceProvider);
  final unlockCodeService = ref.watch(unlockCodeServiceProvider);
  final countdownDuration = ref.watch(countdownDurationProvider);

  // Get current dog from dog provider
  final guardDog = ref.watch(dogProvider);

  // If no dog yet, create temporary placeholder
  if (guardDog == null) {
    final now = DateTime.now();
    final tempDog = Dog(
      id: 'temp',
      name: 'Guard',
      breed: DogBreed.germanShepherd,
      stats: const DogStats(hunger: 80, happiness: 80, energy: 80, loyalty: 80),
      personality: const DogPersonality(protective: true, brave: true),
      createdAt: now,
      lastInteraction: now,
    );

    final barkService = ref.watch(barkAudioServiceProvider(tempDog));

    final service = AlarmService(
      sensorService: sensorService,
      barkService: barkService,
      backgroundService: backgroundService,
      notificationService: notificationService,
      persistenceService: persistenceService,
      unlockCodeService: unlockCodeService,
      countdownDuration: countdownDuration,
      guardDog: tempDog,
    );

    ref.onDispose(() => service.dispose());

    return service;
  }

  final barkService = ref.watch(barkAudioServiceProvider(guardDog));

  final service = AlarmService(
    sensorService: sensorService,
    barkService: barkService,
    backgroundService: backgroundService,
    notificationService: notificationService,
    persistenceService: persistenceService,
    unlockCodeService: unlockCodeService,
    countdownDuration: countdownDuration,
    guardDog: guardDog,
  );

  ref.onDispose(() => service.dispose());

  return service;
});

/// Provider for current alarm state
final alarmStateProvider = StreamProvider<AlarmState>((ref) {
  final service = ref.watch(alarmServiceProvider);
  return service.alarmStateStream;
});
