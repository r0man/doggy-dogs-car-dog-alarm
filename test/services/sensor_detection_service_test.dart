import 'package:flutter_test/flutter_test.dart';
import 'package:doggy_dogs_car_alarm/models/sensor_data.dart';
import 'package:doggy_dogs_car_alarm/services/sensor_detection_service.dart';

void main() {
  TestWidgetsFlutterBinding.ensureInitialized();

  group('SensorDetectionService', () {
    late SensorDetectionService service;

    setUp(() {
      service = SensorDetectionService(sensitivity: AlarmSensitivity.medium);
    });

    tearDown(() {
      service.dispose();
    });

    test('service initializes with correct sensitivity', () {
      // Assert
      expect(service.sensitivity, AlarmSensitivity.medium);
      expect(service.isMonitoring, false);
    });

    test('motionEvents stream is available', () {
      // Assert
      expect(service.motionEvents, isA<Stream<MotionEvent>>());
    });

    test('dispose cleans up resources when not monitoring', () {
      // Act
      service.dispose();

      // Assert
      expect(service.isMonitoring, false);
    });

    test('different sensitivity levels have correct thresholds', () {
      // Arrange
      final lowSensitivity = SensorDetectionService(
        sensitivity: AlarmSensitivity.low,
      );
      final highSensitivity = SensorDetectionService(
        sensitivity: AlarmSensitivity.high,
      );

      // Assert
      expect(
        lowSensitivity.sensitivity.accelerometerThreshold >
            highSensitivity.sensitivity.accelerometerThreshold,
        true,
      );
      expect(
        lowSensitivity.sensitivity.gyroscopeThreshold >
            highSensitivity.sensitivity.gyroscopeThreshold,
        true,
      );

      // Cleanup
      lowSensitivity.dispose();
      highSensitivity.dispose();
    });
  });

  group('SensorDetectionService integration', () {
    test('service can be created with custom sensitivity', () {
      // Arrange
      const customSensitivity = AlarmSensitivity(
        name: 'Custom',
        threshold: 0.4,
        accelerometerThreshold: 2.0,
        gyroscopeThreshold: 1.2,
      );

      // Act
      final service = SensorDetectionService(sensitivity: customSensitivity);

      // Assert
      expect(service.sensitivity.name, 'Custom');
      expect(service.sensitivity.threshold, 0.4);
      expect(service.sensitivity.accelerometerThreshold, 2.0);
      expect(service.sensitivity.gyroscopeThreshold, 1.2);

      // Cleanup
      service.dispose();
    });

    test('multiple services can coexist', () {
      // Act
      final service1 = SensorDetectionService(
        sensitivity: AlarmSensitivity.low,
      );
      final service2 = SensorDetectionService(
        sensitivity: AlarmSensitivity.high,
      );

      // Assert
      expect(service1.sensitivity, AlarmSensitivity.low);
      expect(service2.sensitivity, AlarmSensitivity.high);
      expect(service1.isMonitoring, false);
      expect(service2.isMonitoring, false);

      // Cleanup
      service1.dispose();
      service2.dispose();
    });

    // Note: The following tests require platform channels and sensors which are not available
    // in the test environment without extensive mocking. These would be better as integration tests.

    // test('startMonitoring changes monitoring state', () async {
    //   final service = SensorDetectionService(sensitivity: AlarmSensitivity.medium);
    //   expect(service.isMonitoring, false);
    //   await service.startMonitoring();
    //   expect(service.isMonitoring, true);
    //   await service.stopMonitoring();
    //   service.dispose();
    // });

    test('motionEvents stream broadcasts events', () async {
      // Arrange
      final service = SensorDetectionService(
        sensitivity: AlarmSensitivity.medium,
      );

      // Act
      final stream = service.motionEvents;

      // Assert
      expect(stream, isA<Stream<MotionEvent>>());
      expect(stream.isBroadcast, true);

      // Cleanup
      service.dispose();
    });
  });

  group('SensorReading calculations', () {
    test('magnitude calculation for accelerometer reading', () {
      // Arrange - Simulating device acceleration
      final reading = SensorReading(
        x: 0.0,
        y: 9.8, // Gravity on y-axis when device is upright
        z: 0.0,
        timestamp: DateTime.now(),
        type: SensorType.accelerometer,
      );

      // Act
      final magnitude = reading.magnitude;

      // Assert
      expect(magnitude, closeTo(9.8, 0.01));
    });

    test('difference calculation between readings', () {
      // Arrange - Device tilted
      final baseline = SensorReading(
        x: 0.0,
        y: 9.8,
        z: 0.0,
        timestamp: DateTime.now(),
        type: SensorType.accelerometer,
      );

      // Device experiences shake
      final shaken = SensorReading(
        x: 3.0,
        y: 9.8,
        z: 4.0,
        timestamp: DateTime.now(),
        type: SensorType.accelerometer,
      );

      // Act
      final difference = shaken.differenceFrom(baseline);

      // Assert
      expect(difference, 5.0); // sqrt(3^2 + 0^2 + 4^2) = 5
    });
  });

  group('MotionEvent detection thresholds', () {
    test('low sensitivity requires larger motion', () {
      // Arrange
      final smallMotion = MotionEvent(
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

      // Assert
      expect(
        smallMotion.shouldTriggerAlarm(AlarmSensitivity.low),
        false,
      ); // 0.5 < 0.7
      expect(
        smallMotion.shouldTriggerAlarm(AlarmSensitivity.medium),
        true,
      ); // 0.5 >= 0.5
    });

    test('high sensitivity triggers on small motion', () {
      // Arrange
      final tinyMotion = MotionEvent(
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

      // Assert
      expect(
        tinyMotion.shouldTriggerAlarm(AlarmSensitivity.low),
        false,
      ); // 0.2 < 0.7
      expect(
        tinyMotion.shouldTriggerAlarm(AlarmSensitivity.medium),
        false,
      ); // 0.2 < 0.5
      expect(
        tinyMotion.shouldTriggerAlarm(AlarmSensitivity.high),
        false,
      ); // 0.2 < 0.3
      expect(
        tinyMotion.shouldTriggerAlarm(AlarmSensitivity.veryHigh),
        true,
      ); // 0.2 >= 0.15
    });

    test('impact motion always triggers alarm', () {
      // Arrange
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

      // Assert - High intensity should trigger all sensitivity levels
      expect(impact.shouldTriggerAlarm(AlarmSensitivity.low), true);
      expect(impact.shouldTriggerAlarm(AlarmSensitivity.medium), true);
      expect(impact.shouldTriggerAlarm(AlarmSensitivity.high), true);
      expect(impact.shouldTriggerAlarm(AlarmSensitivity.veryHigh), true);
    });
  });
}
