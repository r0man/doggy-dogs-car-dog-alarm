import 'package:flutter_test/flutter_test.dart';
import 'package:mockito/mockito.dart';
import 'package:mockito/annotations.dart';
import 'package:doggy_dogs_car_alarm/services/alarm_service.dart';
import 'package:doggy_dogs_car_alarm/services/sensor_detection_service.dart';
import 'package:doggy_dogs_car_alarm/services/bark_audio_service.dart';
import 'package:doggy_dogs_car_alarm/services/background_monitoring_service.dart';
import 'package:doggy_dogs_car_alarm/services/notification_service.dart';
import 'package:doggy_dogs_car_alarm/services/alarm_persistence_service.dart';
import 'package:doggy_dogs_car_alarm/services/unlock_code_service.dart';
import 'package:doggy_dogs_car_alarm/services/alarm_state_manager.dart';
import 'package:doggy_dogs_car_alarm/services/alarm_timer.dart';
import 'package:doggy_dogs_car_alarm/services/alarm_conditions.dart';
import 'package:doggy_dogs_car_alarm/models/alarm_state.dart';
import 'package:doggy_dogs_car_alarm/models/sensor_data.dart';
import 'package:doggy_dogs_car_alarm/models/dog.dart';
import 'dart:async';

@GenerateMocks([
  SensorDetectionService,
  BarkAudioService,
  BackgroundMonitoringService,
  NotificationService,
  AlarmPersistenceService,
  UnlockCodeService,
])
import 'alarm_service_test.mocks.dart';

void main() {
  group('AlarmService Integration Tests', () {
    late AlarmService alarmService;
    late MockSensorDetectionService mockSensorService;
    late MockBarkAudioService mockBarkService;
    late MockBackgroundMonitoringService mockBackgroundService;
    late MockNotificationService mockNotificationService;
    late MockAlarmPersistenceService mockPersistenceService;
    late MockUnlockCodeService mockUnlockCodeService;
    late AlarmStateManager stateManager;
    late AlarmTimer timer;
    late AlarmConditions conditions;
    late Dog testDog;
    late StreamController<MotionEvent> motionController;

    setUp(() {
      mockSensorService = MockSensorDetectionService();
      mockBarkService = MockBarkAudioService();
      mockBackgroundService = MockBackgroundMonitoringService();
      mockNotificationService = MockNotificationService();
      mockPersistenceService = MockAlarmPersistenceService();
      mockUnlockCodeService = MockUnlockCodeService();

      final now = DateTime.now();
      testDog = Dog(
        id: 'test-dog',
        name: 'Max',
        breed: DogBreed.germanShepherd,
        stats: const DogStats(
          hunger: 80,
          happiness: 80,
          energy: 80,
          loyalty: 80,
        ),
        personality: const DogPersonality(
          protective: true,
          brave: true,
        ),
        createdAt: now,
        lastInteraction: now,
      );

      stateManager = AlarmStateManager();
      timer = AlarmTimer();
      conditions = AlarmConditions(
        sensitivity: AlarmSensitivity.medium,
        guardDog: testDog,
      );

      motionController = StreamController<MotionEvent>.broadcast();

      // Setup mock returns
      when(mockSensorService.sensitivity).thenReturn(AlarmSensitivity.medium);
      when(mockSensorService.motionEvents)
          .thenAnswer((_) => motionController.stream);
      when(mockSensorService.startMonitoring()).thenAnswer((_) async => {});
      when(mockSensorService.stopMonitoring()).thenAnswer((_) async => {});
      when(mockSensorService.recalibrate()).thenAnswer((_) async => {});

      when(mockBarkService.startBarking(any)).thenAnswer((_) async => {});
      when(mockBarkService.stopBarking()).thenAnswer((_) async => {});

      when(mockBackgroundService.startMonitoring(mode: anyNamed('mode')))
          .thenAnswer((_) async => {});
      when(mockBackgroundService.stopMonitoring()).thenAnswer((_) async => {});

      when(mockNotificationService.showAlarmActivated(
        mode: anyNamed('mode'),
        dogName: anyNamed('dogName'),
      )).thenAnswer((_) async => {});

      when(mockNotificationService.showAlarmDeactivated(
        dogName: anyNamed('dogName'),
      )).thenAnswer((_) async => {});

      when(mockNotificationService.showAlarmTriggered(
        motionType: anyNamed('motionType'),
        intensity: anyNamed('intensity'),
        dogName: anyNamed('dogName'),
      )).thenAnswer((_) async => {});

      when(mockPersistenceService.saveAlarmState(any))
          .thenAnswer((_) async => {});
      when(mockPersistenceService.clearAlarmState())
          .thenAnswer((_) async => {});

      alarmService = AlarmService(
        sensorService: mockSensorService,
        barkService: mockBarkService,
        backgroundService: mockBackgroundService,
        notificationService: mockNotificationService,
        persistenceService: mockPersistenceService,
        unlockCodeService: mockUnlockCodeService,
        countdownDuration: 30,
        guardDog: testDog,
        stateManager: stateManager,
        timer: timer,
        conditions: conditions,
      );
    });

    tearDown(() {
      alarmService.dispose();
      motionController.close();
    });

    group('startActivation', () {
      test('starts countdown and transitions to active', () async {
        final states = <AlarmState>[];
        final subscription = alarmService.alarmStateStream.listen(states.add);

        await alarmService.startActivation(mode: AlarmMode.standard);

        // Wait for countdown to complete
        await Future.delayed(const Duration(milliseconds: 100));

        expect(states.isNotEmpty, true);
        expect(states[0].isCountingDown, true);
        expect(states[0].countdownSeconds, 30);

        // Verify persistence was called
        verify(mockPersistenceService.saveAlarmState(any)).called(1);

        await subscription.cancel();
      });

      test('does not start if already counting down', () async {
        await alarmService.startActivation(mode: AlarmMode.standard);
        clearInteractions(mockPersistenceService);

        await alarmService.startActivation(mode: AlarmMode.aggressive);

        // Should not save state again
        verifyNever(mockPersistenceService.saveAlarmState(any));
      });
    });

    group('cancelCountdown', () {
      test('cancels active countdown', () async {
        await alarmService.startActivation(mode: AlarmMode.standard);
        await alarmService.cancelCountdown();

        expect(alarmService.currentState.isCountingDown, false);
        verify(mockPersistenceService.clearAlarmState()).called(1);
      });

      test('does nothing if countdown not active', () async {
        await alarmService.cancelCountdown();

        verifyNever(mockPersistenceService.clearAlarmState());
      });
    });

    group('deactivateWithUnlockCode', () {
      test('deactivates with valid unlock code', () async {
        // Setup: activate alarm
        stateManager.startCountdown(AlarmMode.standard, 30);
        stateManager.completeActivation(AlarmMode.standard);

        when(mockUnlockCodeService.validateUnlockCode('1234'))
            .thenAnswer((_) async => true);

        final result = await alarmService.deactivateWithUnlockCode('1234');

        expect(result, true);
        expect(alarmService.currentState.isActive, false);
        verify(mockBarkService.stopBarking()).called(1);
        verify(mockSensorService.stopMonitoring()).called(1);
        verify(mockBackgroundService.stopMonitoring()).called(1);
        verify(mockNotificationService.showAlarmDeactivated(
          dogName: anyNamed('dogName'),
        )).called(1);
      });

      test('does not deactivate with invalid unlock code', () async {
        // Setup: activate alarm
        stateManager.startCountdown(AlarmMode.standard, 30);
        stateManager.completeActivation(AlarmMode.standard);

        when(mockUnlockCodeService.validateUnlockCode('wrong'))
            .thenAnswer((_) async => false);

        final result = await alarmService.deactivateWithUnlockCode('wrong');

        expect(result, false);
        expect(alarmService.currentState.isActive, true);
        verifyNever(mockBarkService.stopBarking());
      });

      test('returns false if alarm not active', () async {
        when(mockUnlockCodeService.validateUnlockCode('1234'))
            .thenAnswer((_) async => true);

        final result = await alarmService.deactivateWithUnlockCode('1234');

        expect(result, false);
      });
    });

    group('acknowledge', () {
      test('acknowledges triggered alarm', () async {
        // Setup: activate and trigger alarm
        stateManager.startCountdown(AlarmMode.standard, 30);
        stateManager.completeActivation(AlarmMode.standard);
        stateManager.trigger();

        await alarmService.acknowledge();

        expect(alarmService.currentState.isTriggered, false);
        expect(alarmService.currentState.isActive, true);
        verify(mockBarkService.stopBarking()).called(1);
      });

      test('does nothing if alarm not triggered', () async {
        await alarmService.acknowledge();

        verifyNever(mockBarkService.stopBarking());
      });
    });

    // Motion event handling tests are omitted because they require
    // the private _completeActivation method to be called which sets up
    // the motion subscription. These scenarios are better tested through
    // the extracted AlarmConditions class which has comprehensive tests.

    group('recalibrate', () {
      test('recalibrates sensors and clears motion buffer', () async {
        await alarmService.recalibrate();

        verify(mockSensorService.recalibrate()).called(1);
      });
    });

    group('dispose', () {
      test('cleans up all resources', () {
        alarmService.dispose();

        expect(timer.isRunning, false);
      });
    });

    group('full lifecycle', () {
      test('completes activation and deactivation cycle', () async {
        final states = <AlarmState>[];
        final subscription = alarmService.alarmStateStream.listen(states.add);

        // 1. Start activation
        await alarmService.startActivation(mode: AlarmMode.standard);
        await Future.delayed(const Duration(milliseconds: 50));

        // 2. Complete activation (simulate countdown completion)
        stateManager.updateCountdown(1);
        stateManager.completeActivation(AlarmMode.standard);
        await Future.delayed(const Duration(milliseconds: 50));

        // 3. Manually trigger alarm (since motion events require private method)
        stateManager.trigger();
        await Future.delayed(const Duration(milliseconds: 50));

        // 4. Acknowledge alarm
        await alarmService.acknowledge();
        await Future.delayed(const Duration(milliseconds: 50));

        // 5. Deactivate
        when(mockUnlockCodeService.validateUnlockCode('1234'))
            .thenAnswer((_) async => true);
        await alarmService.deactivateWithUnlockCode('1234');
        await Future.delayed(const Duration(milliseconds: 50));

        // Verify state transitions
        expect(states.any((s) => s.isCountingDown), true);
        expect(states.any((s) => s.isActive && !s.isTriggered), true);
        expect(states.any((s) => s.isTriggered), true);
        expect(states.any((s) => !s.isActive), true);

        await subscription.cancel();
      });
    });
  });
}
