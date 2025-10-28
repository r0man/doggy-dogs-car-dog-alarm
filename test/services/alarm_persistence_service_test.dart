import 'package:flutter_test/flutter_test.dart';
import 'package:shared_preferences/shared_preferences.dart';
import 'package:doggy_dogs_car_alarm/services/alarm_persistence_service.dart';
import 'package:doggy_dogs_car_alarm/models/alarm_state.dart';

void main() {
  TestWidgetsFlutterBinding.ensureInitialized();

  group('AlarmPersistenceService', () {
    late AlarmPersistenceService service;

    setUp(() async {
      // Clear shared preferences before each test
      SharedPreferences.setMockInitialValues({});
      service = AlarmPersistenceService();
      await service.initialize();
    });

    test('should initialize successfully', () async {
      expect(service, isNotNull);
    });

    test('should save and load alarm state', () async {
      // Create alarm state
      final state = const AlarmState(
        status: AlarmStatus.active,
        mode: AlarmMode.standard,
      );

      // Save state
      await service.saveAlarmState(state);

      // Load state
      final loadedState = await service.loadAlarmState();

      // Verify
      expect(loadedState, isNotNull);
      expect(loadedState!.status, AlarmStatus.active);
      expect(loadedState.mode, AlarmMode.standard);
      expect(loadedState.isActive, true);
    });

    test('should save and load alarm state with timestamps', () async {
      // Create alarm state with timestamps
      final now = DateTime.now();
      final state = AlarmState(
        status: AlarmStatus.triggered,
        mode: AlarmMode.aggressive,
        activatedAt: now,
        triggeredAt: now.add(Duration(seconds: 30)),
      );

      // Save state
      await service.saveAlarmState(state);

      // Load state
      final loadedState = await service.loadAlarmState();

      // Verify
      expect(loadedState, isNotNull);
      expect(loadedState!.status, AlarmStatus.triggered);
      expect(loadedState.mode, AlarmMode.aggressive);
      expect(loadedState.activatedAt, isNotNull);
      expect(loadedState.triggeredAt, isNotNull);

      // Times should be within 1 second (accounting for serialization)
      expect(
        loadedState.activatedAt!.difference(now).inSeconds.abs(),
        lessThan(2),
      );
    });

    test('should return null when loading non-existent alarm state', () async {
      final loadedState = await service.loadAlarmState();
      expect(loadedState, isNull);
    });

    test('should clear alarm state', () async {
      // Save state
      final state = const AlarmState(
        status: AlarmStatus.active,
        mode: AlarmMode.standard,
      );
      await service.saveAlarmState(state);

      // Verify saved
      final loaded1 = await service.loadAlarmState();
      expect(loaded1, isNotNull);

      // Clear state
      await service.clearAlarmState();

      // Verify cleared
      final loaded2 = await service.loadAlarmState();
      expect(loaded2, isNull);
    });

    test('should save and load current dog ID', () async {
      const dogId = 'dog-123';

      // Save dog ID
      await service.saveCurrentDogId(dogId);

      // Load dog ID
      final loadedDogId = await service.loadCurrentDogId();

      // Verify
      expect(loadedDogId, dogId);
    });

    test('should save and load sensitivity', () async {
      const sensitivity = 'high';

      // Save sensitivity
      await service.saveSensitivity(sensitivity);

      // Load sensitivity
      final loadedSensitivity = await service.loadSensitivity();

      // Verify
      expect(loadedSensitivity, sensitivity);
    });

    test('wasAlarmActive should return correct status', () async {
      // Initially no active alarm
      bool wasActive = await service.wasAlarmActive();
      expect(wasActive, false);

      // Save active alarm
      final state = const AlarmState(
        status: AlarmStatus.active,
        mode: AlarmMode.standard,
      );
      await service.saveAlarmState(state);

      // Check again
      wasActive = await service.wasAlarmActive();
      expect(wasActive, true);
    });

    test('getActivationDuration should return correct duration', () async {
      // Save alarm state with activation time 1 minute ago
      final activatedAt = DateTime.now().subtract(Duration(minutes: 1));
      final state = AlarmState(
        status: AlarmStatus.active,
        mode: AlarmMode.standard,
        activatedAt: activatedAt,
      );
      await service.saveAlarmState(state);

      // Get duration
      final duration = await service.getActivationDuration();

      // Verify (should be approximately 1 minute)
      expect(duration, isNotNull);
      expect(duration!.inSeconds, greaterThan(55)); // At least 55 seconds
      expect(duration.inSeconds, lessThan(65)); // At most 65 seconds
    });

    test('getActivationDuration should return null when not activated', () async {
      final duration = await service.getActivationDuration();
      expect(duration, isNull);
    });

    test('clearAll should remove all persisted data', () async {
      // Save various data
      await service.saveAlarmState(const AlarmState(
        status: AlarmStatus.active,
        mode: AlarmMode.standard,
      ));
      await service.saveCurrentDogId('dog-123');
      await service.saveSensitivity('high');

      // Verify saved
      expect(await service.loadAlarmState(), isNotNull);
      expect(await service.loadCurrentDogId(), isNotNull);
      expect(await service.loadSensitivity(), isNotNull);

      // Clear all
      await service.clearAll();

      // Verify all cleared
      expect(await service.loadAlarmState(), isNull);
      expect(await service.loadCurrentDogId(), isNull);
      expect(await service.loadSensitivity(), isNull);
    });

    test('should handle different alarm modes', () async {
      for (final mode in AlarmMode.values) {
        final state = AlarmState(
          status: AlarmStatus.active,
          mode: mode,
        );

        await service.saveAlarmState(state);
        final loaded = await service.loadAlarmState();

        expect(loaded, isNotNull);
        expect(loaded!.mode, mode);
      }
    });

    test('should handle different alarm statuses', () async {
      for (final status in AlarmStatus.values) {
        final state = AlarmState(
          status: status,
          mode: AlarmMode.standard,
        );

        await service.saveAlarmState(state);
        final loaded = await service.loadAlarmState();

        expect(loaded, isNotNull);
        expect(loaded!.status, status);
      }
    });
  });
}
