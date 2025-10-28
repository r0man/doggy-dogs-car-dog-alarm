import 'dart:async';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import '../models/alarm_state.dart';
import '../models/sensor_data.dart';
import '../models/dog.dart';
import 'sensor_detection_service.dart';

/// Service for managing the car alarm system
class AlarmService {
  final SensorDetectionService sensorService;
  final Dog guardDog;

  StreamSubscription<MotionEvent>? _motionSubscription;

  final _alarmStateController = StreamController<AlarmState>.broadcast();
  AlarmState _currentState = const AlarmState();

  // Threat verification
  final List<MotionEvent> _recentMotions = [];
  Timer? _verificationTimer;
  static const _verificationWindow = Duration(seconds: 3);
  static const _motionsToConfirm = 2;

  AlarmService({
    required this.sensorService,
    required this.guardDog,
  });

  /// Stream of alarm state changes
  Stream<AlarmState> get alarmStateStream => _alarmStateController.stream;

  /// Current alarm state
  AlarmState get currentState => _currentState;

  /// Activate the alarm
  Future<void> activate({AlarmMode mode = AlarmMode.standard}) async {
    if (_currentState.isActive) return;

    // Update state
    _currentState = _currentState.activate(mode);
    _alarmStateController.add(_currentState);

    // Start monitoring sensors
    await sensorService.startMonitoring();

    // Subscribe to motion events
    _motionSubscription = sensorService.motionEvents.listen(
      _handleMotionEvent,
      onError: (error) {
        _alarmStateController.addError(error);
      },
    );
  }

  /// Deactivate the alarm
  Future<void> deactivate() async {
    if (!_currentState.isActive) return;

    // Update state
    _currentState = _currentState.deactivate();
    _alarmStateController.add(_currentState);

    // Stop monitoring sensors
    await _motionSubscription?.cancel();
    await sensorService.stopMonitoring();

    // Clear verification data
    _recentMotions.clear();
    _verificationTimer?.cancel();
  }

  /// Acknowledge and silence a triggered alarm
  void acknowledge() {
    if (!_currentState.isTriggered) return;

    _currentState = _currentState.acknowledge();
    _alarmStateController.add(_currentState);

    // Clear recent motions after acknowledgment
    _recentMotions.clear();
  }

  /// Handle motion event from sensors
  void _handleMotionEvent(MotionEvent event) {
    if (!_currentState.isActive) return;

    // Check if motion exceeds sensitivity threshold
    if (!event.shouldTriggerAlarm(sensorService.sensitivity)) return;

    // Apply dog effectiveness modifier
    final effectiveIntensity = event.intensity * (guardDog.effectiveness / 100);

    // Aggressive mode: trigger immediately
    if (_currentState.mode == AlarmMode.aggressive) {
      _triggerAlarm(event);
      return;
    }

    // Standard/Stealth mode: verify threat first
    if (_currentState.mode.hasDelayedResponse) {
      _verifyThreat(event);
      return;
    }

    // If intensity is very high, trigger immediately regardless of mode
    if (effectiveIntensity > 0.8) {
      _triggerAlarm(event);
    }
  }

  /// Verify if motion is a real threat
  void _verifyThreat(MotionEvent event) {
    // Add to recent motions
    _recentMotions.add(event);

    // Remove old motions outside verification window
    final cutoff = DateTime.now().subtract(_verificationWindow);
    _recentMotions.removeWhere((m) => m.timestamp.isBefore(cutoff));

    // If enough motions detected, trigger alarm
    if (_recentMotions.length >= _motionsToConfirm) {
      _triggerAlarm(event);
      _recentMotions.clear();
      return;
    }

    // Start verification timer if not already running
    _verificationTimer?.cancel();
    _verificationTimer = Timer(_verificationWindow, () {
      // If we didn't get enough confirmations, clear the buffer
      _recentMotions.clear();
    });
  }

  /// Trigger the alarm
  void _triggerAlarm(MotionEvent event) {
    if (_currentState.isTriggered) return; // Already triggered

    _currentState = _currentState.trigger();
    _alarmStateController.add(_currentState);

    // TODO: Trigger bark sound system
    // TODO: Send notification
    // TODO: Log event
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
    _alarmStateController.close();
  }
}

/// Provider for the alarm service
final alarmServiceProvider = Provider<AlarmService>((ref) {
  final sensitivity = ref.watch(alarmSensitivityProvider);
  final sensorService = ref.watch(
    sensorDetectionServiceProvider(sensitivity),
  );

  // TODO: Get actual dog from dog provider
  final now = DateTime.now();
  final guardDog = Dog(
    id: 'temp',
    name: 'Guard',
    breed: DogBreed.germanShepherd,
    stats: const DogStats(hunger: 80, happiness: 80, energy: 80, loyalty: 80),
    personality: const DogPersonality(protective: true, brave: true),
    createdAt: now,
    lastInteraction: now,
  );

  final service = AlarmService(
    sensorService: sensorService,
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
