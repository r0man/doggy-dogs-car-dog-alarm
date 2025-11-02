import 'package:flutter_test/flutter_test.dart';
import 'package:doggy_dogs_car_alarm/models/app_settings.dart';

void main() {
  group('AppSettings Tests', () {
    test('creates default settings', () {
      const settings = AppSettings();

      expect(settings.countdownDuration, 10);
      expect(settings.sensitivityLevel, 'medium');
      expect(settings.barkVolume, 0.8);
      expect(settings.notificationsEnabled, true);
      expect(settings.batteryOptimizationEnabled, true);
    });

    test('copyWith updates specified values', () {
      const settings = AppSettings();
      final updated = settings.copyWith(
        countdownDuration: 60,
        sensitivityLevel: 'high',
        barkVolume: 0.5,
      );

      expect(updated.countdownDuration, 60);
      expect(updated.sensitivityLevel, 'high');
      expect(updated.barkVolume, 0.5);
      expect(updated.notificationsEnabled, true); // unchanged
      expect(updated.batteryOptimizationEnabled, true); // unchanged
    });

    test('toJson creates correct map', () {
      const settings = AppSettings(
        countdownDuration: 45,
        sensitivityLevel: 'veryHigh',
        barkVolume: 0.9,
        notificationsEnabled: false,
        batteryOptimizationEnabled: false,
      );

      final json = settings.toJson();

      expect(json['countdownDuration'], 45);
      expect(json['sensitivityLevel'], 'veryHigh');
      expect(json['barkVolume'], 0.9);
      expect(json['notificationsEnabled'], false);
      expect(json['batteryOptimizationEnabled'], false);
    });

    test('fromJson creates correct settings', () {
      final json = {
        'countdownDuration': 90,
        'sensitivityLevel': 'low',
        'barkVolume': 0.6,
        'notificationsEnabled': true,
        'batteryOptimizationEnabled': false,
      };

      final settings = AppSettings.fromJson(json);

      expect(settings.countdownDuration, 90);
      expect(settings.sensitivityLevel, 'low');
      expect(settings.barkVolume, 0.6);
      expect(settings.notificationsEnabled, true);
      expect(settings.batteryOptimizationEnabled, false);
    });

    test('fromJson uses defaults for missing values', () {
      final json = <String, dynamic>{};

      final settings = AppSettings.fromJson(json);

      expect(settings.countdownDuration, 10);
      expect(settings.sensitivityLevel, 'medium');
      expect(settings.barkVolume, 0.8);
      expect(settings.notificationsEnabled, true);
      expect(settings.batteryOptimizationEnabled, true);
    });

    test('isValidCountdownDuration validates range', () {
      expect(
          const AppSettings(countdownDuration: 10).isValidCountdownDuration(),
          true);
      expect(
          const AppSettings(countdownDuration: 60).isValidCountdownDuration(),
          true);
      expect(
          const AppSettings(countdownDuration: 120).isValidCountdownDuration(),
          true);
      expect(const AppSettings(countdownDuration: 9).isValidCountdownDuration(),
          false);
      expect(
          const AppSettings(countdownDuration: 121).isValidCountdownDuration(),
          false);
    });

    test('isValidBarkVolume validates range', () {
      expect(const AppSettings(barkVolume: 0.0).isValidBarkVolume(), true);
      expect(const AppSettings(barkVolume: 0.5).isValidBarkVolume(), true);
      expect(const AppSettings(barkVolume: 1.0).isValidBarkVolume(), true);
      expect(const AppSettings(barkVolume: -0.1).isValidBarkVolume(), false);
      expect(const AppSettings(barkVolume: 1.1).isValidBarkVolume(), false);
    });

    test('isValidSensitivityLevel validates options', () {
      expect(
          const AppSettings(sensitivityLevel: 'low').isValidSensitivityLevel(),
          true);
      expect(
          const AppSettings(sensitivityLevel: 'medium')
              .isValidSensitivityLevel(),
          true);
      expect(
          const AppSettings(sensitivityLevel: 'high').isValidSensitivityLevel(),
          true);
      expect(
          const AppSettings(sensitivityLevel: 'veryHigh')
              .isValidSensitivityLevel(),
          true);
      expect(
          const AppSettings(sensitivityLevel: 'invalid')
              .isValidSensitivityLevel(),
          false);
    });

    test('isValid checks all validations', () {
      const valid = AppSettings();
      expect(valid.isValid(), true);

      const invalidCountdown = AppSettings(countdownDuration: 200);
      expect(invalidCountdown.isValid(), false);

      const invalidVolume = AppSettings(barkVolume: 2.0);
      expect(invalidVolume.isValid(), false);

      const invalidSensitivity = AppSettings(sensitivityLevel: 'extreme');
      expect(invalidSensitivity.isValid(), false);
    });

    test('equality operator works correctly', () {
      const settings1 =
          AppSettings(countdownDuration: 45, sensitivityLevel: 'high');
      const settings2 =
          AppSettings(countdownDuration: 45, sensitivityLevel: 'high');
      const settings3 =
          AppSettings(countdownDuration: 60, sensitivityLevel: 'high');

      expect(settings1 == settings2, true);
      expect(settings1 == settings3, false);
    });

    test('hashCode is consistent', () {
      const settings1 = AppSettings(countdownDuration: 45);
      const settings2 = AppSettings(countdownDuration: 45);

      expect(settings1.hashCode, settings2.hashCode);
    });

    test('toString returns readable representation', () {
      const settings = AppSettings(
        countdownDuration: 45,
        sensitivityLevel: 'high',
      );

      final str = settings.toString();
      expect(str, contains('countdownDuration: 45'));
      expect(str, contains('sensitivityLevel: high'));
    });

    test('round-trip JSON serialization', () {
      const original = AppSettings(
        countdownDuration: 75,
        sensitivityLevel: 'veryHigh',
        barkVolume: 0.7,
        notificationsEnabled: false,
        batteryOptimizationEnabled: true,
      );

      final json = original.toJson();
      final restored = AppSettings.fromJson(json);

      expect(restored, original);
    });
  });
}
