import 'package:flutter_test/flutter_test.dart';
import 'package:doggy_dogs_car_alarm/models/sensor_data.dart';

void main() {
  group('SensorReading', () {
    test('calculates magnitude correctly', () {
      // Arrange
      final reading = SensorReading(
        x: 3.0,
        y: 4.0,
        z: 0.0,
        timestamp: DateTime.now(),
        type: SensorType.accelerometer,
      );

      // Act
      final magnitude = reading.magnitude;

      // Assert
      expect(magnitude, 5.0); // sqrt(3^2 + 4^2 + 0^2) = 5
    });

    test('calculates difference from another reading correctly', () {
      // Arrange
      final reading1 = SensorReading(
        x: 1.0,
        y: 2.0,
        z: 3.0,
        timestamp: DateTime.now(),
        type: SensorType.accelerometer,
      );

      final reading2 = SensorReading(
        x: 4.0,
        y: 6.0,
        z: 3.0,
        timestamp: DateTime.now(),
        type: SensorType.accelerometer,
      );

      // Act
      final difference = reading2.differenceFrom(reading1);

      // Assert
      // sqrt((4-1)^2 + (6-2)^2 + (3-3)^2) = sqrt(9 + 16 + 0) = 5
      expect(difference, 5.0);
    });

    test('copyWith creates new instance with updated values', () {
      // Arrange
      final original = SensorReading(
        x: 1.0,
        y: 2.0,
        z: 3.0,
        timestamp: DateTime.now(),
        type: SensorType.accelerometer,
      );

      // Act
      final copy = original.copyWith(x: 10.0, y: 20.0);

      // Assert
      expect(copy.x, 10.0);
      expect(copy.y, 20.0);
      expect(copy.z, 3.0); // Unchanged
      expect(copy.type, SensorType.accelerometer); // Unchanged
    });

    test('copyWith can update z and timestamp', () {
      // Arrange
      final original = SensorReading(
        x: 1.0,
        y: 2.0,
        z: 3.0,
        timestamp: DateTime(2024, 1, 1),
        type: SensorType.accelerometer,
      );

      final newTimestamp = DateTime(2024, 12, 31);

      // Act
      final copy = original.copyWith(z: 30.0, timestamp: newTimestamp);

      // Assert
      expect(copy.x, 1.0); // Unchanged
      expect(copy.y, 2.0); // Unchanged
      expect(copy.z, 30.0);
      expect(copy.timestamp, newTimestamp);
      expect(copy.type, SensorType.accelerometer); // Unchanged
    });

    test('copyWith can update type', () {
      // Arrange
      final original = SensorReading(
        x: 1.0,
        y: 2.0,
        z: 3.0,
        timestamp: DateTime.now(),
        type: SensorType.accelerometer,
      );

      // Act
      final copy = original.copyWith(type: SensorType.gyroscope);

      // Assert
      expect(copy.x, 1.0);
      expect(copy.y, 2.0);
      expect(copy.z, 3.0);
      expect(copy.type, SensorType.gyroscope);
    });

    test('toString returns readable representation', () {
      // Arrange
      final reading = SensorReading(
        x: 1.5,
        y: 2.5,
        z: 3.5,
        timestamp: DateTime.now(),
        type: SensorType.gyroscope,
      );

      // Act
      final str = reading.toString();

      // Assert
      expect(str, contains('SensorReading'));
      expect(str, contains('gyroscope'));
      expect(str, contains('x=1.5'));
      expect(str, contains('y=2.5'));
      expect(str, contains('z=3.5'));
    });
  });

  group('MotionEvent', () {
    test('shouldTriggerAlarm returns true when intensity exceeds threshold',
        () {
      // Arrange
      final event = MotionEvent(
        intensity: 0.6,
        type: MotionType.shake,
        timestamp: DateTime.now(),
        triggerReading: SensorReading(
          x: 5.0,
          y: 5.0,
          z: 5.0,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
      );

      // Act
      final shouldTrigger = event.shouldTriggerAlarm(AlarmSensitivity.medium);

      // Assert
      expect(shouldTrigger, true); // 0.6 >= 0.5 (medium threshold)
    });

    test('shouldTriggerAlarm returns false when intensity below threshold', () {
      // Arrange
      final event = MotionEvent(
        intensity: 0.4,
        type: MotionType.subtle,
        timestamp: DateTime.now(),
        triggerReading: SensorReading(
          x: 1.0,
          y: 1.0,
          z: 1.0,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
      );

      // Act
      final shouldTrigger = event.shouldTriggerAlarm(AlarmSensitivity.medium);

      // Assert
      expect(shouldTrigger, false); // 0.4 < 0.5 (medium threshold)
    });

    test('toString returns readable representation', () {
      // Arrange
      final event = MotionEvent(
        intensity: 0.8,
        type: MotionType.impact,
        timestamp: DateTime.now(),
        triggerReading: SensorReading(
          x: 10.0,
          y: 10.0,
          z: 10.0,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
      );

      // Act
      final str = event.toString();

      // Assert
      expect(str, contains('MotionEvent'));
      expect(str, contains('impact'));
      expect(str, contains('intensity=0.8'));
    });
  });

  group('AlarmSensitivity', () {
    test('predefined sensitivity levels have correct thresholds', () {
      // Assert
      expect(AlarmSensitivity.low.threshold, 0.7);
      expect(AlarmSensitivity.medium.threshold, 0.5);
      expect(AlarmSensitivity.high.threshold, 0.3);
      expect(AlarmSensitivity.veryHigh.threshold, 0.15);
    });

    test('low sensitivity has highest thresholds', () {
      // Assert
      expect(
        AlarmSensitivity.low.accelerometerThreshold >
            AlarmSensitivity.medium.accelerometerThreshold,
        true,
      );
      expect(
        AlarmSensitivity.low.gyroscopeThreshold >
            AlarmSensitivity.medium.gyroscopeThreshold,
        true,
      );
    });

    test('very high sensitivity has lowest thresholds', () {
      // Assert
      expect(
        AlarmSensitivity.veryHigh.accelerometerThreshold <
            AlarmSensitivity.high.accelerometerThreshold,
        true,
      );
      expect(
        AlarmSensitivity.veryHigh.gyroscopeThreshold <
            AlarmSensitivity.high.gyroscopeThreshold,
        true,
      );
    });

    test('all predefined sensitivities are available', () {
      // Assert
      expect(AlarmSensitivity.all.length, 4);
      expect(AlarmSensitivity.all, contains(AlarmSensitivity.low));
      expect(AlarmSensitivity.all, contains(AlarmSensitivity.medium));
      expect(AlarmSensitivity.all, contains(AlarmSensitivity.high));
      expect(AlarmSensitivity.all, contains(AlarmSensitivity.veryHigh));
    });

    test('copyWith creates new instance with updated values', () {
      // Arrange
      const original = AlarmSensitivity.medium;

      // Act
      final copy = original.copyWith(
        name: 'Custom',
        threshold: 0.6,
      );

      // Assert
      expect(copy.name, 'Custom');
      expect(copy.threshold, 0.6);
      expect(copy.accelerometerThreshold, original.accelerometerThreshold);
      expect(copy.gyroscopeThreshold, original.gyroscopeThreshold);
    });

    test('copyWith can update all parameters', () {
      // Arrange
      const original = AlarmSensitivity.medium;

      // Act
      final copy = original.copyWith(
        name: 'Super Sensitive',
        threshold: 0.1,
        accelerometerThreshold: 0.5,
        gyroscopeThreshold: 0.3,
      );

      // Assert
      expect(copy.name, 'Super Sensitive');
      expect(copy.threshold, 0.1);
      expect(copy.accelerometerThreshold, 0.5);
      expect(copy.gyroscopeThreshold, 0.3);
    });
  });

  group('MotionType', () {
    test('all motion types are defined', () {
      // Assert
      expect(MotionType.values.length, 4);
      expect(MotionType.values, contains(MotionType.shake));
      expect(MotionType.values, contains(MotionType.tilt));
      expect(MotionType.values, contains(MotionType.impact));
      expect(MotionType.values, contains(MotionType.subtle));
    });
  });

  group('SensorType', () {
    test('all sensor types are defined', () {
      // Assert
      expect(SensorType.values.length, 2);
      expect(SensorType.values, contains(SensorType.accelerometer));
      expect(SensorType.values, contains(SensorType.gyroscope));
    });
  });
}
