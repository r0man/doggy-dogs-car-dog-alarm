import 'dart:async';
import 'package:flutter_test/flutter_test.dart';
import 'package:doggy_dogs_car_alarm/models/sensor_data.dart';
import 'package:doggy_dogs_car_alarm/services/sensor_detection_service.dart';
import 'package:sensors_plus/sensors_plus.dart';

// Test extension to expose private methods for testing
extension SensorDetectionServiceTest on SensorDetectionService {
  // Expose private methods for testing
  void handleAccelerometerEventTest(AccelerometerEvent event) {
    // This will call the private _handleAccelerometerEvent method
    // We need to use a workaround since we can't directly access private methods
  }

  void handleGyroscopeEventTest(GyroscopeEvent event) {
    // This will call the private _handleGyroscopeEvent method
  }
}

void main() {
  TestWidgetsFlutterBinding.ensureInitialized();

  group('SensorDetectionService - Initialization', () {
    late SensorDetectionService service;

    setUp(() {
      service = SensorDetectionService(sensitivity: AlarmSensitivity.medium);
    });

    tearDown(() {
      service.dispose();
    });

    test('service initializes with correct sensitivity', () {
      expect(service.sensitivity, AlarmSensitivity.medium);
      expect(service.isMonitoring, false);
    });

    test('service initializes with low sensitivity', () {
      final lowService =
          SensorDetectionService(sensitivity: AlarmSensitivity.low);
      expect(lowService.sensitivity, AlarmSensitivity.low);
      expect(lowService.sensitivity.accelerometerThreshold, 5.0);
      expect(lowService.sensitivity.gyroscopeThreshold, 2.0);
      lowService.dispose();
    });

    test('service initializes with high sensitivity', () {
      final highService =
          SensorDetectionService(sensitivity: AlarmSensitivity.high);
      expect(highService.sensitivity, AlarmSensitivity.high);
      expect(highService.sensitivity.accelerometerThreshold, 1.5);
      expect(highService.sensitivity.gyroscopeThreshold, 1.0);
      highService.dispose();
    });

    test('service initializes with very high sensitivity', () {
      final veryHighService =
          SensorDetectionService(sensitivity: AlarmSensitivity.veryHigh);
      expect(veryHighService.sensitivity, AlarmSensitivity.veryHigh);
      expect(veryHighService.sensitivity.accelerometerThreshold, 0.8);
      expect(veryHighService.sensitivity.gyroscopeThreshold, 0.5);
      veryHighService.dispose();
    });

    test('motionEvents stream is available and broadcast', () {
      expect(service.motionEvents, isA<Stream<MotionEvent>>());
      expect(service.motionEvents.isBroadcast, true);
    });

    test('isMonitoring returns false initially', () {
      expect(service.isMonitoring, false);
    });
  });

  group('SensorDetectionService - Lifecycle', () {
    test('dispose cleans up resources when not monitoring', () {
      final service = SensorDetectionService();
      expect(() => service.dispose(), returnsNormally);
      expect(service.isMonitoring, false);
    });

    test('dispose can be called multiple times', () {
      final service = SensorDetectionService();
      service.dispose();
      expect(() => service.dispose(), returnsNormally);
    });

    test('recalibrate completes successfully', () async {
      final service = SensorDetectionService();
      await expectLater(service.recalibrate(), completes);
      service.dispose();
    });

    test('stopMonitoring when not monitoring completes without error',
        () async {
      final service = SensorDetectionService();
      expect(service.isMonitoring, false);
      await expectLater(service.stopMonitoring(), completes);
      expect(service.isMonitoring, false);
      service.dispose();
    });

    test('stopMonitoring can be called multiple times', () async {
      final service = SensorDetectionService();
      await expectLater(service.stopMonitoring(), completes);
      await expectLater(service.stopMonitoring(), completes);
      service.dispose();
    });
  });

  group('SensorDetectionService - Motion Event Stream', () {
    test('multiple listeners can subscribe to motion events', () async {
      final service = SensorDetectionService();

      final subscription1 = service.motionEvents.listen((_) {});
      final subscription2 = service.motionEvents.listen((_) {});

      expect(subscription1, isA<StreamSubscription<MotionEvent>>());
      expect(subscription2, isA<StreamSubscription<MotionEvent>>());

      await subscription1.cancel();
      await subscription2.cancel();
      service.dispose();
    });

    test('motion events stream remains open after listener cancels', () async {
      final service = SensorDetectionService();

      final subscription = service.motionEvents.listen((_) {});
      await subscription.cancel();

      // Stream should still be available for new listeners
      expect(service.motionEvents, isA<Stream<MotionEvent>>());
      service.dispose();
    });
  });

  group('SensorDetectionService - Sensitivity Levels', () {
    test('low sensitivity has higher thresholds than medium', () {
      final lowService =
          SensorDetectionService(sensitivity: AlarmSensitivity.low);
      final mediumService =
          SensorDetectionService(sensitivity: AlarmSensitivity.medium);

      expect(
        lowService.sensitivity.accelerometerThreshold,
        greaterThan(mediumService.sensitivity.accelerometerThreshold),
      );
      expect(
        lowService.sensitivity.gyroscopeThreshold,
        greaterThan(mediumService.sensitivity.gyroscopeThreshold),
      );

      lowService.dispose();
      mediumService.dispose();
    });

    test('medium sensitivity has higher thresholds than high', () {
      final mediumService =
          SensorDetectionService(sensitivity: AlarmSensitivity.medium);
      final highService =
          SensorDetectionService(sensitivity: AlarmSensitivity.high);

      expect(
        mediumService.sensitivity.accelerometerThreshold,
        greaterThan(highService.sensitivity.accelerometerThreshold),
      );
      expect(
        mediumService.sensitivity.gyroscopeThreshold,
        greaterThan(highService.sensitivity.gyroscopeThreshold),
      );

      mediumService.dispose();
      highService.dispose();
    });

    test('high sensitivity has higher thresholds than very high', () {
      final highService =
          SensorDetectionService(sensitivity: AlarmSensitivity.high);
      final veryHighService =
          SensorDetectionService(sensitivity: AlarmSensitivity.veryHigh);

      expect(
        highService.sensitivity.accelerometerThreshold,
        greaterThan(veryHighService.sensitivity.accelerometerThreshold),
      );
      expect(
        highService.sensitivity.gyroscopeThreshold,
        greaterThan(veryHighService.sensitivity.gyroscopeThreshold),
      );

      highService.dispose();
      veryHighService.dispose();
    });

    test('custom sensitivity can be created', () {
      const customSensitivity = AlarmSensitivity(
        name: 'Custom',
        threshold: 0.4,
        accelerometerThreshold: 2.5,
        gyroscopeThreshold: 1.2,
      );

      final service = SensorDetectionService(sensitivity: customSensitivity);

      expect(service.sensitivity.name, 'Custom');
      expect(service.sensitivity.threshold, 0.4);
      expect(service.sensitivity.accelerometerThreshold, 2.5);
      expect(service.sensitivity.gyroscopeThreshold, 1.2);

      service.dispose();
    });
  });

  group('SensorReading - Data Model', () {
    test('magnitude calculation for zero vector', () {
      final reading = SensorReading(
        x: 0.0,
        y: 0.0,
        z: 0.0,
        timestamp: DateTime.now(),
        type: SensorType.accelerometer,
      );

      expect(reading.magnitude, 0.0);
    });

    test('magnitude calculation for unit vector', () {
      final reading = SensorReading(
        x: 1.0,
        y: 0.0,
        z: 0.0,
        timestamp: DateTime.now(),
        type: SensorType.accelerometer,
      );

      expect(reading.magnitude, 1.0);
    });

    test('magnitude calculation for gravity vector (device upright)', () {
      final reading = SensorReading(
        x: 0.0,
        y: 9.8,
        z: 0.0,
        timestamp: DateTime.now(),
        type: SensorType.accelerometer,
      );

      expect(reading.magnitude, closeTo(9.8, 0.01));
    });

    test('magnitude calculation for 3D vector', () {
      final reading = SensorReading(
        x: 3.0,
        y: 4.0,
        z: 0.0,
        timestamp: DateTime.now(),
        type: SensorType.accelerometer,
      );

      // sqrt(3^2 + 4^2 + 0^2) = 5
      expect(reading.magnitude, 5.0);
    });

    test('difference calculation between identical readings', () {
      final timestamp = DateTime.now();
      final reading1 = SensorReading(
        x: 1.0,
        y: 2.0,
        z: 3.0,
        timestamp: timestamp,
        type: SensorType.accelerometer,
      );
      final reading2 = SensorReading(
        x: 1.0,
        y: 2.0,
        z: 3.0,
        timestamp: timestamp,
        type: SensorType.accelerometer,
      );

      expect(reading1.differenceFrom(reading2), 0.0);
    });

    test('difference calculation between different readings', () {
      final baseline = SensorReading(
        x: 0.0,
        y: 9.8,
        z: 0.0,
        timestamp: DateTime.now(),
        type: SensorType.accelerometer,
      );

      final shaken = SensorReading(
        x: 3.0,
        y: 9.8,
        z: 4.0,
        timestamp: DateTime.now(),
        type: SensorType.accelerometer,
      );

      // sqrt(3^2 + 0^2 + 4^2) = 5
      expect(shaken.differenceFrom(baseline), 5.0);
    });

    test('difference calculation with negative values', () {
      final reading1 = SensorReading(
        x: -1.0,
        y: -2.0,
        z: -3.0,
        timestamp: DateTime.now(),
        type: SensorType.accelerometer,
      );

      final reading2 = SensorReading(
        x: 1.0,
        y: 2.0,
        z: 3.0,
        timestamp: DateTime.now(),
        type: SensorType.accelerometer,
      );

      // sqrt((1-(-1))^2 + (2-(-2))^2 + (3-(-3))^2) = sqrt(4+16+36) = sqrt(56)
      expect(reading1.differenceFrom(reading2), closeTo(7.48, 0.01));
    });

    test('copyWith creates new reading with updated values', () {
      final original = SensorReading(
        x: 1.0,
        y: 2.0,
        z: 3.0,
        timestamp: DateTime(2024, 1, 1),
        type: SensorType.accelerometer,
      );

      final modified = original.copyWith(x: 5.0, y: 6.0);

      expect(modified.x, 5.0);
      expect(modified.y, 6.0);
      expect(modified.z, 3.0); // Unchanged
      expect(modified.type, SensorType.accelerometer); // Unchanged
    });

    test('copyWith can change sensor type', () {
      final original = SensorReading(
        x: 1.0,
        y: 2.0,
        z: 3.0,
        timestamp: DateTime.now(),
        type: SensorType.accelerometer,
      );

      final modified = original.copyWith(type: SensorType.gyroscope);

      expect(modified.type, SensorType.gyroscope);
      expect(modified.x, original.x);
      expect(modified.y, original.y);
      expect(modified.z, original.z);
    });
  });

  group('MotionEvent - Detection and Classification', () {
    test('shouldTriggerAlarm with low sensitivity and low intensity', () {
      final event = MotionEvent(
        intensity: 0.5,
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

      // Low sensitivity threshold is 0.7, so 0.5 should not trigger
      expect(event.shouldTriggerAlarm(AlarmSensitivity.low), false);
    });

    test('shouldTriggerAlarm with low sensitivity and high intensity', () {
      final event = MotionEvent(
        intensity: 0.8,
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

      // Low sensitivity threshold is 0.7, so 0.8 should trigger
      expect(event.shouldTriggerAlarm(AlarmSensitivity.low), true);
    });

    test('shouldTriggerAlarm with medium sensitivity', () {
      final event = MotionEvent(
        intensity: 0.5,
        type: MotionType.tilt,
        timestamp: DateTime.now(),
        triggerReading: SensorReading(
          x: 2.0,
          y: 2.0,
          z: 2.0,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
      );

      // Medium sensitivity threshold is 0.5, so 0.5 should trigger (>=)
      expect(event.shouldTriggerAlarm(AlarmSensitivity.medium), true);
      // But not low sensitivity (0.7)
      expect(event.shouldTriggerAlarm(AlarmSensitivity.low), false);
    });

    test('shouldTriggerAlarm with high sensitivity', () {
      final event = MotionEvent(
        intensity: 0.35,
        type: MotionType.subtle,
        timestamp: DateTime.now(),
        triggerReading: SensorReading(
          x: 1.5,
          y: 1.5,
          z: 1.5,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
      );

      // High sensitivity threshold is 0.3, so 0.35 should trigger
      expect(event.shouldTriggerAlarm(AlarmSensitivity.high), true);
      // But not medium (0.5) or low (0.7)
      expect(event.shouldTriggerAlarm(AlarmSensitivity.medium), false);
      expect(event.shouldTriggerAlarm(AlarmSensitivity.low), false);
    });

    test('shouldTriggerAlarm with very high sensitivity', () {
      final event = MotionEvent(
        intensity: 0.2,
        type: MotionType.subtle,
        timestamp: DateTime.now(),
        triggerReading: SensorReading(
          x: 0.5,
          y: 0.5,
          z: 0.5,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
      );

      // Very high sensitivity threshold is 0.15, so 0.2 should trigger
      expect(event.shouldTriggerAlarm(AlarmSensitivity.veryHigh), true);
      // But not any other sensitivity level
      expect(event.shouldTriggerAlarm(AlarmSensitivity.high), false);
      expect(event.shouldTriggerAlarm(AlarmSensitivity.medium), false);
      expect(event.shouldTriggerAlarm(AlarmSensitivity.low), false);
    });

    test('impact motion with high intensity triggers all sensitivities', () {
      final impact = MotionEvent(
        intensity: 0.95,
        type: MotionType.impact,
        timestamp: DateTime.now(),
        triggerReading: SensorReading(
          x: 20.0,
          y: 20.0,
          z: 20.0,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
      );

      expect(impact.shouldTriggerAlarm(AlarmSensitivity.low), true);
      expect(impact.shouldTriggerAlarm(AlarmSensitivity.medium), true);
      expect(impact.shouldTriggerAlarm(AlarmSensitivity.high), true);
      expect(impact.shouldTriggerAlarm(AlarmSensitivity.veryHigh), true);
    });

    test('motion event at exact threshold triggers alarm', () {
      final event = MotionEvent(
        intensity: 0.7,
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

      // At exact threshold (0.7 for low sensitivity)
      expect(event.shouldTriggerAlarm(AlarmSensitivity.low), true);
    });

    test('motion event just below threshold does not trigger alarm', () {
      final event = MotionEvent(
        intensity: 0.69,
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

      // Just below threshold (0.7 for low sensitivity)
      expect(event.shouldTriggerAlarm(AlarmSensitivity.low), false);
    });
  });

  group('MotionType - Classification', () {
    test('shake motion type', () {
      final event = MotionEvent(
        intensity: 0.6,
        type: MotionType.shake,
        timestamp: DateTime.now(),
        triggerReading: SensorReading(
          x: 4.0,
          y: 4.0,
          z: 4.0,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
      );

      expect(event.type, MotionType.shake);
    });

    test('tilt motion type', () {
      final event = MotionEvent(
        intensity: 0.4,
        type: MotionType.tilt,
        timestamp: DateTime.now(),
        triggerReading: SensorReading(
          x: 2.0,
          y: 8.0,
          z: 1.0,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
      );

      expect(event.type, MotionType.tilt);
    });

    test('impact motion type', () {
      final event = MotionEvent(
        intensity: 0.9,
        type: MotionType.impact,
        timestamp: DateTime.now(),
        triggerReading: SensorReading(
          x: 15.0,
          y: 15.0,
          z: 15.0,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
      );

      expect(event.type, MotionType.impact);
    });

    test('subtle motion type', () {
      final event = MotionEvent(
        intensity: 0.2,
        type: MotionType.subtle,
        timestamp: DateTime.now(),
        triggerReading: SensorReading(
          x: 0.5,
          y: 0.5,
          z: 0.5,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
      );

      expect(event.type, MotionType.subtle);
    });
  });

  group('AlarmSensitivity - Configuration', () {
    test('low sensitivity configuration', () {
      expect(AlarmSensitivity.low.name, 'Low');
      expect(AlarmSensitivity.low.threshold, 0.7);
      expect(AlarmSensitivity.low.accelerometerThreshold, 5.0);
      expect(AlarmSensitivity.low.gyroscopeThreshold, 2.0);
    });

    test('medium sensitivity configuration', () {
      expect(AlarmSensitivity.medium.name, 'Medium');
      expect(AlarmSensitivity.medium.threshold, 0.5);
      expect(AlarmSensitivity.medium.accelerometerThreshold, 3.0);
      expect(AlarmSensitivity.medium.gyroscopeThreshold, 1.5);
    });

    test('high sensitivity configuration', () {
      expect(AlarmSensitivity.high.name, 'High');
      expect(AlarmSensitivity.high.threshold, 0.3);
      expect(AlarmSensitivity.high.accelerometerThreshold, 1.5);
      expect(AlarmSensitivity.high.gyroscopeThreshold, 1.0);
    });

    test('very high sensitivity configuration', () {
      expect(AlarmSensitivity.veryHigh.name, 'Very High');
      expect(AlarmSensitivity.veryHigh.threshold, 0.15);
      expect(AlarmSensitivity.veryHigh.accelerometerThreshold, 0.8);
      expect(AlarmSensitivity.veryHigh.gyroscopeThreshold, 0.5);
    });

    test('all sensitivities are in order', () {
      expect(AlarmSensitivity.low.threshold,
          greaterThan(AlarmSensitivity.medium.threshold));
      expect(AlarmSensitivity.medium.threshold,
          greaterThan(AlarmSensitivity.high.threshold));
      expect(AlarmSensitivity.high.threshold,
          greaterThan(AlarmSensitivity.veryHigh.threshold));
    });

    test('copyWith creates new sensitivity with updated values', () {
      final custom = AlarmSensitivity.medium.copyWith(
        name: 'Custom Medium',
        threshold: 0.6,
      );

      expect(custom.name, 'Custom Medium');
      expect(custom.threshold, 0.6);
      expect(custom.accelerometerThreshold,
          AlarmSensitivity.medium.accelerometerThreshold);
      expect(custom.gyroscopeThreshold,
          AlarmSensitivity.medium.gyroscopeThreshold);
    });

    test('all sensitivity levels are defined', () {
      expect(AlarmSensitivity.all.length, 4);
      expect(AlarmSensitivity.all[0], AlarmSensitivity.low);
      expect(AlarmSensitivity.all[1], AlarmSensitivity.medium);
      expect(AlarmSensitivity.all[2], AlarmSensitivity.high);
      expect(AlarmSensitivity.all[3], AlarmSensitivity.veryHigh);
    });
  });

  group('SensorDetectionService - Multiple Instances', () {
    test('multiple services can coexist independently', () {
      final service1 =
          SensorDetectionService(sensitivity: AlarmSensitivity.low);
      final service2 =
          SensorDetectionService(sensitivity: AlarmSensitivity.high);

      expect(service1.sensitivity, AlarmSensitivity.low);
      expect(service2.sensitivity, AlarmSensitivity.high);
      expect(service1.isMonitoring, false);
      expect(service2.isMonitoring, false);

      service1.dispose();
      service2.dispose();
    });

    test('disposing one service does not affect others', () {
      final service1 =
          SensorDetectionService(sensitivity: AlarmSensitivity.low);
      final service2 =
          SensorDetectionService(sensitivity: AlarmSensitivity.high);

      service1.dispose();

      // service2 should still be functional
      expect(service2.isMonitoring, false);
      expect(service2.motionEvents, isA<Stream<MotionEvent>>());

      service2.dispose();
    });
  });

  group('SensorDetectionService - Edge Cases', () {
    test('service handles zero sensitivity values', () {
      const zeroSensitivity = AlarmSensitivity(
        name: 'Zero',
        threshold: 0.0,
        accelerometerThreshold: 0.0,
        gyroscopeThreshold: 0.0,
      );

      final service = SensorDetectionService(sensitivity: zeroSensitivity);
      expect(service.sensitivity.accelerometerThreshold, 0.0);
      expect(service.sensitivity.gyroscopeThreshold, 0.0);
      service.dispose();
    });

    test('service handles very large sensitivity values', () {
      const largeSensitivity = AlarmSensitivity(
        name: 'Large',
        threshold: 1.0,
        accelerometerThreshold: 1000.0,
        gyroscopeThreshold: 1000.0,
      );

      final service = SensorDetectionService(sensitivity: largeSensitivity);
      expect(service.sensitivity.accelerometerThreshold, 1000.0);
      expect(service.sensitivity.gyroscopeThreshold, 1000.0);
      service.dispose();
    });

    test('motion event with zero intensity', () {
      final event = MotionEvent(
        intensity: 0.0,
        type: MotionType.subtle,
        timestamp: DateTime.now(),
        triggerReading: SensorReading(
          x: 0.0,
          y: 0.0,
          z: 0.0,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
      );

      expect(event.shouldTriggerAlarm(AlarmSensitivity.low), false);
      expect(event.shouldTriggerAlarm(AlarmSensitivity.medium), false);
      expect(event.shouldTriggerAlarm(AlarmSensitivity.high), false);
      expect(event.shouldTriggerAlarm(AlarmSensitivity.veryHigh), false);
    });

    test('motion event with maximum intensity', () {
      final event = MotionEvent(
        intensity: 1.0,
        type: MotionType.impact,
        timestamp: DateTime.now(),
        triggerReading: SensorReading(
          x: 100.0,
          y: 100.0,
          z: 100.0,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
      );

      expect(event.shouldTriggerAlarm(AlarmSensitivity.low), true);
      expect(event.shouldTriggerAlarm(AlarmSensitivity.medium), true);
      expect(event.shouldTriggerAlarm(AlarmSensitivity.high), true);
      expect(event.shouldTriggerAlarm(AlarmSensitivity.veryHigh), true);
    });
  });

  group('SensorType - Enum', () {
    test('sensor types are distinct', () {
      expect(SensorType.accelerometer, isNot(SensorType.gyroscope));
    });

    test('accelerometer reading has correct type', () {
      final reading = SensorReading(
        x: 1.0,
        y: 2.0,
        z: 3.0,
        timestamp: DateTime.now(),
        type: SensorType.accelerometer,
      );

      expect(reading.type, SensorType.accelerometer);
    });

    test('gyroscope reading has correct type', () {
      final reading = SensorReading(
        x: 1.0,
        y: 2.0,
        z: 3.0,
        timestamp: DateTime.now(),
        type: SensorType.gyroscope,
      );

      expect(reading.type, SensorType.gyroscope);
    });
  });

  group('SensorDetectionService - String Representations', () {
    test('SensorReading toString includes all fields', () {
      final reading = SensorReading(
        x: 1.5,
        y: 2.5,
        z: 3.5,
        timestamp: DateTime.now(),
        type: SensorType.accelerometer,
      );

      final str = reading.toString();
      expect(str, contains('SensorReading'));
      expect(str, contains('accelerometer'));
      expect(str, contains('x=1.5'));
      expect(str, contains('y=2.5'));
      expect(str, contains('z=3.5'));
      expect(str, contains('magnitude'));
    });

    test('MotionEvent toString includes all fields', () {
      final event = MotionEvent(
        intensity: 0.75,
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

      final str = event.toString();
      expect(str, contains('MotionEvent'));
      expect(str, contains('shake'));
      expect(str, contains('intensity=0.75'));
    });
  });
}
