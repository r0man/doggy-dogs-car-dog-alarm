import 'package:flutter_test/flutter_test.dart';
import 'package:shared_preferences/shared_preferences.dart';
import 'package:doggy_dogs_car_alarm/models/app_settings.dart';
import 'package:doggy_dogs_car_alarm/models/sensor_data.dart';
import 'package:doggy_dogs_car_alarm/services/app_settings_service.dart';

void main() {
  group('AppSettingsService', () {
    late SharedPreferences prefs;
    late AppSettingsService service;

    setUp(() async {
      SharedPreferences.setMockInitialValues({});
      prefs = await SharedPreferences.getInstance();
      service = AppSettingsService(prefs);
    });

    tearDown(() async {
      await prefs.clear();
    });

    test('loadSettings returns defaults when no saved settings', () async {
      final settings = await service.loadSettings();

      expect(settings.countdownDuration, 30);
      expect(settings.sensitivityLevel, 'medium');
      expect(settings.barkVolume, 0.8);
    });

    test('saveSettings persists settings', () async {
      const settings = AppSettings(
        countdownDuration: 60,
        sensitivityLevel: 'high',
        barkVolume: 0.5,
      );

      await service.saveSettings(settings);

      final loaded = await service.loadSettings();
      expect(loaded.countdownDuration, 60);
      expect(loaded.sensitivityLevel, 'high');
      expect(loaded.barkVolume, 0.5);
    });

    test('saveSettings throws on invalid settings', () async {
      const invalidSettings = AppSettings(
        countdownDuration: 200, // Invalid: > 120
      );

      expect(
        () => service.saveSettings(invalidSettings),
        throwsArgumentError,
      );
    });

    test('clearSettings removes all settings', () async {
      const settings = AppSettings(countdownDuration: 60);
      await service.saveSettings(settings);

      await service.clearSettings();

      final loaded = await service.loadSettings();
      expect(loaded.countdownDuration, 30); // Back to default
    });

    test('loadSettings handles corrupted JSON gracefully', () async {
      await prefs.setString('app_settings', 'invalid json');

      final settings = await service.loadSettings();
      expect(settings.countdownDuration, 30); // Returns defaults
    });

    test('getAlarmSensitivity converts correctly', () {
      const lowSettings = AppSettings(sensitivityLevel: 'low');
      const mediumSettings = AppSettings(sensitivityLevel: 'medium');
      const highSettings = AppSettings(sensitivityLevel: 'high');
      const veryHighSettings = AppSettings(sensitivityLevel: 'veryHigh');

      expect(service.getAlarmSensitivity(lowSettings), AlarmSensitivity.low);
      expect(
          service.getAlarmSensitivity(mediumSettings), AlarmSensitivity.medium);
      expect(service.getAlarmSensitivity(highSettings), AlarmSensitivity.high);
      expect(service.getAlarmSensitivity(veryHighSettings),
          AlarmSensitivity.veryHigh);
    });

    test('getAlarmSensitivity defaults to medium for unknown value', () {
      const unknownSettings = AppSettings(sensitivityLevel: 'unknown');

      expect(service.getAlarmSensitivity(unknownSettings),
          AlarmSensitivity.medium);
    });

    test('getSensitivityName converts correctly', () {
      expect(service.getSensitivityName(AlarmSensitivity.low), 'low');
      expect(service.getSensitivityName(AlarmSensitivity.medium), 'medium');
      expect(service.getSensitivityName(AlarmSensitivity.high), 'high');
      expect(service.getSensitivityName(AlarmSensitivity.veryHigh), 'veryHigh');
    });
  });

  group('AppSettingsNotifier', () {
    late SharedPreferences prefs;
    late AppSettingsService service;
    late AppSettingsNotifier notifier;

    setUp(() async {
      SharedPreferences.setMockInitialValues({});
      prefs = await SharedPreferences.getInstance();
      service = AppSettingsService(prefs);
      notifier = AppSettingsNotifier(service);
      // Wait for initial load
      await Future.delayed(const Duration(milliseconds: 100));
    });

    test('initializes with default settings', () {
      expect(notifier.state.countdownDuration, 30);
      expect(notifier.state.sensitivityLevel, 'medium');
    });

    test('setCountdownDuration updates and persists', () async {
      await notifier.setCountdownDuration(60);

      expect(notifier.state.countdownDuration, 60);

      // Verify persistence
      final loaded = await service.loadSettings();
      expect(loaded.countdownDuration, 60);
    });

    test('setCountdownDuration throws on invalid value', () async {
      expect(
        () => notifier.setCountdownDuration(10), // Too low
        throwsArgumentError,
      );

      expect(
        () => notifier.setCountdownDuration(150), // Too high
        throwsArgumentError,
      );
    });

    test('setSensitivityLevel updates and persists', () async {
      await notifier.setSensitivityLevel('high');

      expect(notifier.state.sensitivityLevel, 'high');

      final loaded = await service.loadSettings();
      expect(loaded.sensitivityLevel, 'high');
    });

    test('setSensitivityLevel throws on invalid value', () async {
      expect(
        () => notifier.setSensitivityLevel('invalid'),
        throwsArgumentError,
      );
    });

    test('setBarkVolume updates and persists', () async {
      await notifier.setBarkVolume(0.5);

      expect(notifier.state.barkVolume, 0.5);

      final loaded = await service.loadSettings();
      expect(loaded.barkVolume, 0.5);
    });

    test('setBarkVolume throws on invalid value', () async {
      expect(
        () => notifier.setBarkVolume(-0.1), // Too low
        throwsArgumentError,
      );

      expect(
        () => notifier.setBarkVolume(1.5), // Too high
        throwsArgumentError,
      );
    });

    test('setNotificationsEnabled updates and persists', () async {
      await notifier.setNotificationsEnabled(false);

      expect(notifier.state.notificationsEnabled, isFalse);

      final loaded = await service.loadSettings();
      expect(loaded.notificationsEnabled, isFalse);
    });

    test('setBatteryOptimizationEnabled updates and persists', () async {
      await notifier.setBatteryOptimizationEnabled(false);

      expect(notifier.state.batteryOptimizationEnabled, isFalse);

      final loaded = await service.loadSettings();
      expect(loaded.batteryOptimizationEnabled, isFalse);
    });

    test('resetToDefaults clears settings', () async {
      await notifier.setCountdownDuration(90);
      await notifier.setSensitivityLevel('veryHigh');

      await notifier.resetToDefaults();

      expect(notifier.state.countdownDuration, 30);
      expect(notifier.state.sensitivityLevel, 'medium');
    });
  });
}
