import 'package:flutter_test/flutter_test.dart';
import 'package:doggy_dogs_car_alarm/models/sensor_data.dart';

void main() {
  group('SensorReading Tests', () {
    test('creates sensor reading', () {
      final timestamp = DateTime.now();
      final reading = SensorReading(
        x: 1.0,
        y: 2.0,
        z: 3.0,
        timestamp: timestamp,
        type: SensorType.accelerometer,
      );

      expect(reading.x, 1.0);
      expect(reading.y, 2.0);
      expect(reading.z, 3.0);
      expect(reading.timestamp, timestamp);
      expect(reading.type, SensorType.accelerometer);
    });

    test('calculates magnitude correctly', () {
      final reading = SensorReading(
        x: 3.0,
        y: 4.0,
        z: 0.0,
        timestamp: DateTime.now(),
        type: SensorType.accelerometer,
      );

      expect(reading.magnitude, 5.0); // sqrt(3^2 + 4^2 + 0^2) = 5
    });

    test('differenceFrom calculates correctly', () {
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

      final diff = reading1.differenceFrom(reading2);
      expect(diff, 5.0); // sqrt((4-1)^2 + (6-2)^2 + (3-3)^2) = sqrt(9 + 16) = 5
    });

    test('copyWith updates specified values', () {
      final timestamp1 = DateTime.now();
      final timestamp2 = DateTime.now().add(const Duration(seconds: 1));

      final reading = SensorReading(
        x: 1.0,
        y: 2.0,
        z: 3.0,
        timestamp: timestamp1,
        type: SensorType.accelerometer,
      );

      final updated = reading.copyWith(
        x: 5.0,
        timestamp: timestamp2,
      );

      expect(updated.x, 5.0);
      expect(updated.y, 2.0); // unchanged
      expect(updated.z, 3.0); // unchanged
      expect(updated.timestamp, timestamp2);
      expect(updated.type, SensorType.accelerometer); // unchanged
    });

    test('toString returns readable representation', () {
      final reading = SensorReading(
        x: 1.5,
        y: 2.5,
        z: 3.5,
        timestamp: DateTime.now(),
        type: SensorType.gyroscope,
      );

      final str = reading.toString();
      expect(str, contains('SensorReading'));
      expect(str, contains('gyroscope'));
      expect(str, contains('x=1.5'));
      expect(str, contains('y=2.5'));
      expect(str, contains('z=3.5'));
    });
  });

  group('MotionEvent Tests', () {
    test('creates motion event', () {
      final timestamp = DateTime.now();
      final reading = SensorReading(
        x: 1.0,
        y: 2.0,
        z: 3.0,
        timestamp: timestamp,
        type: SensorType.accelerometer,
      );

      final event = MotionEvent(
        intensity: 0.8,
        type: MotionType.shake,
        timestamp: timestamp,
        triggerReading: reading,
      );

      expect(event.intensity, 0.8);
      expect(event.type, MotionType.shake);
      expect(event.timestamp, timestamp);
      expect(event.triggerReading, reading);
    });

    test('shouldTriggerAlarm checks sensitivity threshold', () {
      final event = MotionEvent(
        intensity: 0.6,
        type: MotionType.shake,
        timestamp: DateTime.now(),
        triggerReading: SensorReading(
          x: 1.0,
          y: 2.0,
          z: 3.0,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
      );

      expect(event.shouldTriggerAlarm(AlarmSensitivity.low),
          false); // threshold 0.7
      expect(event.shouldTriggerAlarm(AlarmSensitivity.medium),
          true); // threshold 0.5
      expect(event.shouldTriggerAlarm(AlarmSensitivity.high),
          true); // threshold 0.3
      expect(event.shouldTriggerAlarm(AlarmSensitivity.veryHigh),
          true); // threshold 0.15
    });

    test('toString returns readable representation', () {
      final event = MotionEvent(
        intensity: 0.75,
        type: MotionType.impact,
        timestamp: DateTime.now(),
        triggerReading: SensorReading(
          x: 1.0,
          y: 2.0,
          z: 3.0,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
      );

      final str = event.toString();
      expect(str, contains('MotionEvent'));
      expect(str, contains('impact'));
      expect(str, contains('intensity=0.75'));
    });
  });

  group('AlarmSensitivity Tests', () {
    test('predefined sensitivities have correct values', () {
      expect(AlarmSensitivity.low.name, 'Low');
      expect(AlarmSensitivity.low.threshold, 0.7);
      expect(AlarmSensitivity.low.accelerometerThreshold, 5.0);
      expect(AlarmSensitivity.low.gyroscopeThreshold, 2.0);

      expect(AlarmSensitivity.medium.name, 'Medium');
      expect(AlarmSensitivity.medium.threshold, 0.5);

      expect(AlarmSensitivity.high.name, 'High');
      expect(AlarmSensitivity.high.threshold, 0.3);

      expect(AlarmSensitivity.veryHigh.name, 'Very High');
      expect(AlarmSensitivity.veryHigh.threshold, 0.15);
    });

    test('sensitivity thresholds are in descending order', () {
      expect(AlarmSensitivity.low.threshold > AlarmSensitivity.medium.threshold,
          true);
      expect(
          AlarmSensitivity.medium.threshold > AlarmSensitivity.high.threshold,
          true);
      expect(
          AlarmSensitivity.high.threshold > AlarmSensitivity.veryHigh.threshold,
          true);
    });

    test('all predefined sensitivities are accessible', () {
      expect(AlarmSensitivity.all.length, 4);
      expect(AlarmSensitivity.all.contains(AlarmSensitivity.low), true);
      expect(AlarmSensitivity.all.contains(AlarmSensitivity.medium), true);
      expect(AlarmSensitivity.all.contains(AlarmSensitivity.high), true);
      expect(AlarmSensitivity.all.contains(AlarmSensitivity.veryHigh), true);
    });

    test('copyWith updates specified values', () {
      const sensitivity = AlarmSensitivity.medium;
      final updated = sensitivity.copyWith(
        name: 'Custom',
        threshold: 0.4,
      );

      expect(updated.name, 'Custom');
      expect(updated.threshold, 0.4);
      expect(updated.accelerometerThreshold, 3.0); // unchanged
      expect(updated.gyroscopeThreshold, 1.5); // unchanged
    });

    test('custom sensitivity can be created', () {
      const custom = AlarmSensitivity(
        name: 'Custom',
        threshold: 0.6,
        accelerometerThreshold: 4.0,
        gyroscopeThreshold: 1.8,
      );

      expect(custom.name, 'Custom');
      expect(custom.threshold, 0.6);
      expect(custom.accelerometerThreshold, 4.0);
      expect(custom.gyroscopeThreshold, 1.8);
    });
  });

  group('MotionType Tests', () {
    test('all motion types are defined', () {
      expect(MotionType.values.length, 4);
      expect(MotionType.values.contains(MotionType.shake), true);
      expect(MotionType.values.contains(MotionType.tilt), true);
      expect(MotionType.values.contains(MotionType.impact), true);
      expect(MotionType.values.contains(MotionType.subtle), true);
    });
  });

  group('SensorType Tests', () {
    test('all sensor types are defined', () {
      expect(SensorType.values.length, 2);
      expect(SensorType.values.contains(SensorType.accelerometer), true);
      expect(SensorType.values.contains(SensorType.gyroscope), true);
    });
  });
}
