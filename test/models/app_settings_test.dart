import 'package:flutter_test/flutter_test.dart';
import 'package:doggy_dogs_car_alarm/models/app_settings.dart';

void main() {
  group('AppSettings', () {
    test('creates with default values', () {
      const settings = AppSettings();

      expect(settings.countdownDuration, 30);
      expect(settings.sensitivityLevel, 'medium');
      expect(settings.barkVolume, 0.8);
      expect(settings.notificationsEnabled, isTrue);
      expect(settings.batteryOptimizationEnabled, isTrue);
    });

    test('copyWith creates new instance with updated values', () {
      const settings = AppSettings();
      final updated = settings.copyWith(
        countdownDuration: 60,
        sensitivityLevel: 'high',
      );

      expect(updated.countdownDuration, 60);
      expect(updated.sensitivityLevel, 'high');
      expect(updated.barkVolume, 0.8); // Unchanged
    });

    test('toJson serializes correctly', () {
      const settings = AppSettings(
        countdownDuration: 45,
        sensitivityLevel: 'low',
        barkVolume: 0.5,
        notificationsEnabled: false,
        batteryOptimizationEnabled: false,
      );

      final json = settings.toJson();

      expect(json['countdownDuration'], 45);
      expect(json['sensitivityLevel'], 'low');
      expect(json['barkVolume'], 0.5);
      expect(json['notificationsEnabled'], isFalse);
      expect(json['batteryOptimizationEnabled'], isFalse);
    });

    test('fromJson deserializes correctly', () {
      final json = {
        'countdownDuration': 90,
        'sensitivityLevel': 'veryHigh',
        'barkVolume': 1.0,
        'notificationsEnabled': true,
        'batteryOptimizationEnabled': false,
      };

      final settings = AppSettings.fromJson(json);

      expect(settings.countdownDuration, 90);
      expect(settings.sensitivityLevel, 'veryHigh');
      expect(settings.barkVolume, 1.0);
      expect(settings.notificationsEnabled, isTrue);
      expect(settings.batteryOptimizationEnabled, isFalse);
    });

    test('fromJson uses defaults for missing values', () {
      final json = <String, dynamic>{};
      final settings = AppSettings.fromJson(json);

      expect(settings.countdownDuration, 30);
      expect(settings.sensitivityLevel, 'medium');
      expect(settings.barkVolume, 0.8);
      expect(settings.notificationsEnabled, isTrue);
      expect(settings.batteryOptimizationEnabled, isTrue);
    });

    test('isValidCountdownDuration validates correctly', () {
      const validMin = AppSettings(countdownDuration: 15);
      const validMax = AppSettings(countdownDuration: 120);
      const invalidLow = AppSettings(countdownDuration: 10);
      const invalidHigh = AppSettings(countdownDuration: 121);

      expect(validMin.isValidCountdownDuration(), isTrue);
      expect(validMax.isValidCountdownDuration(), isTrue);
      expect(invalidLow.isValidCountdownDuration(), isFalse);
      expect(invalidHigh.isValidCountdownDuration(), isFalse);
    });

    test('isValidBarkVolume validates correctly', () {
      const validMin = AppSettings(barkVolume: 0.0);
      const validMax = AppSettings(barkVolume: 1.0);
      const invalidLow = AppSettings(barkVolume: -0.1);
      const invalidHigh = AppSettings(barkVolume: 1.1);

      expect(validMin.isValidBarkVolume(), isTrue);
      expect(validMax.isValidBarkVolume(), isTrue);
      expect(invalidLow.isValidBarkVolume(), isFalse);
      expect(invalidHigh.isValidBarkVolume(), isFalse);
    });

    test('isValidSensitivityLevel validates correctly', () {
      const validLow = AppSettings(sensitivityLevel: 'low');
      const validMedium = AppSettings(sensitivityLevel: 'medium');
      const validHigh = AppSettings(sensitivityLevel: 'high');
      const validVeryHigh = AppSettings(sensitivityLevel: 'veryHigh');
      const invalid = AppSettings(sensitivityLevel: 'invalid');

      expect(validLow.isValidSensitivityLevel(), isTrue);
      expect(validMedium.isValidSensitivityLevel(), isTrue);
      expect(validHigh.isValidSensitivityLevel(), isTrue);
      expect(validVeryHigh.isValidSensitivityLevel(), isTrue);
      expect(invalid.isValidSensitivityLevel(), isFalse);
    });

    test('isValid checks all validations', () {
      const valid = AppSettings(
        countdownDuration: 30,
        sensitivityLevel: 'medium',
        barkVolume: 0.5,
      );

      const invalidCountdown = AppSettings(
        countdownDuration: 200,
        sensitivityLevel: 'medium',
        barkVolume: 0.5,
      );

      const invalidSensitivity = AppSettings(
        countdownDuration: 30,
        sensitivityLevel: 'invalid',
        barkVolume: 0.5,
      );

      const invalidVolume = AppSettings(
        countdownDuration: 30,
        sensitivityLevel: 'medium',
        barkVolume: 2.0,
      );

      expect(valid.isValid(), isTrue);
      expect(invalidCountdown.isValid(), isFalse);
      expect(invalidSensitivity.isValid(), isFalse);
      expect(invalidVolume.isValid(), isFalse);
    });

    test('equality works correctly', () {
      const settings1 = AppSettings(countdownDuration: 30);
      const settings2 = AppSettings(countdownDuration: 30);
      const settings3 = AppSettings(countdownDuration: 60);

      expect(settings1, equals(settings2));
      expect(settings1, isNot(equals(settings3)));
    });

    test('hashCode works correctly', () {
      const settings1 = AppSettings(countdownDuration: 30);
      const settings2 = AppSettings(countdownDuration: 30);
      const settings3 = AppSettings(countdownDuration: 60);

      expect(settings1.hashCode, equals(settings2.hashCode));
      expect(settings1.hashCode, isNot(equals(settings3.hashCode)));
    });

    test('toString returns formatted string', () {
      const settings = AppSettings(
        countdownDuration: 45,
        sensitivityLevel: 'high',
        barkVolume: 0.9,
      );

      final str = settings.toString();

      expect(str, contains('countdownDuration: 45'));
      expect(str, contains('sensitivityLevel: high'));
      expect(str, contains('barkVolume: 0.9'));
    });
  });
}
