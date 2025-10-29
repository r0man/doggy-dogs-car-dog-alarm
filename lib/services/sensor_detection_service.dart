import 'dart:async';
import 'dart:math';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:sensors_plus/sensors_plus.dart';
import '../models/sensor_data.dart';
import 'app_settings_service.dart';

/// Service for detecting motion events using device sensors
class SensorDetectionService {
  final AlarmSensitivity sensitivity;

  StreamSubscription<AccelerometerEvent>? _accelerometerSubscription;
  StreamSubscription<GyroscopeEvent>? _gyroscopeSubscription;

  SensorReading? _lastAccelerometerReading;
  SensorReading? _lastGyroscopeReading;

  final _motionEventController = StreamController<MotionEvent>.broadcast();

  bool _isMonitoring = false;

  // Baseline readings for detecting changes
  SensorReading? _accelerometerBaseline;
  SensorReading? _gyroscopeBaseline;

  // Calibration window
  final List<SensorReading> _accelerometerCalibrationBuffer = [];
  final List<SensorReading> _gyroscopeCalibrationBuffer = [];
  static const _calibrationSamples = 10;

  SensorDetectionService({
    this.sensitivity = AlarmSensitivity.medium,
  });

  /// Stream of detected motion events
  Stream<MotionEvent> get motionEvents => _motionEventController.stream;

  /// Check if service is currently monitoring
  bool get isMonitoring => _isMonitoring;

  /// Start monitoring sensors
  Future<void> startMonitoring() async {
    if (_isMonitoring) return;

    _isMonitoring = true;
    _resetCalibration();

    // Monitor accelerometer
    _accelerometerSubscription = accelerometerEventStream().listen(
      _handleAccelerometerEvent,
      onError: (error) {
        _motionEventController.addError(error);
      },
    );

    // Monitor gyroscope
    _gyroscopeSubscription = gyroscopeEventStream().listen(
      _handleGyroscopeEvent,
      onError: (error) {
        _motionEventController.addError(error);
      },
    );
  }

  /// Stop monitoring sensors
  Future<void> stopMonitoring() async {
    if (!_isMonitoring) return;

    _isMonitoring = false;

    await _accelerometerSubscription?.cancel();
    await _gyroscopeSubscription?.cancel();

    _accelerometerSubscription = null;
    _gyroscopeSubscription = null;

    _lastAccelerometerReading = null;
    _lastGyroscopeReading = null;
    _accelerometerBaseline = null;
    _gyroscopeBaseline = null;
  }

  /// Dispose of resources
  void dispose() {
    stopMonitoring();
    _motionEventController.close();
  }

  /// Reset calibration and establish new baseline
  void _resetCalibration() {
    _accelerometerCalibrationBuffer.clear();
    _gyroscopeCalibrationBuffer.clear();
    _accelerometerBaseline = null;
    _gyroscopeBaseline = null;
  }

  /// Handle accelerometer events
  void _handleAccelerometerEvent(AccelerometerEvent event) {
    final reading = SensorReading(
      x: event.x,
      y: event.y,
      z: event.z,
      timestamp: DateTime.now(),
      type: SensorType.accelerometer,
    );

    // Calibration phase
    if (_accelerometerBaseline == null) {
      _accelerometerCalibrationBuffer.add(reading);

      if (_accelerometerCalibrationBuffer.length >= _calibrationSamples) {
        _accelerometerBaseline = _calculateAverageReading(
          _accelerometerCalibrationBuffer,
          SensorType.accelerometer,
        );
        _accelerometerCalibrationBuffer.clear();
      }
      return;
    }

    // Detect motion
    if (_lastAccelerometerReading != null) {
      _detectMotion(reading, _lastAccelerometerReading!, _accelerometerBaseline!);
    }

    _lastAccelerometerReading = reading;
  }

  /// Handle gyroscope events
  void _handleGyroscopeEvent(GyroscopeEvent event) {
    final reading = SensorReading(
      x: event.x,
      y: event.y,
      z: event.z,
      timestamp: DateTime.now(),
      type: SensorType.gyroscope,
    );

    // Calibration phase
    if (_gyroscopeBaseline == null) {
      _gyroscopeCalibrationBuffer.add(reading);

      if (_gyroscopeCalibrationBuffer.length >= _calibrationSamples) {
        _gyroscopeBaseline = _calculateAverageReading(
          _gyroscopeCalibrationBuffer,
          SensorType.gyroscope,
        );
        _gyroscopeCalibrationBuffer.clear();
      }
      return;
    }

    // Detect motion
    if (_lastGyroscopeReading != null) {
      _detectMotion(reading, _lastGyroscopeReading!, _gyroscopeBaseline!);
    }

    _lastGyroscopeReading = reading;
  }

  /// Calculate average of sensor readings
  SensorReading _calculateAverageReading(
    List<SensorReading> readings,
    SensorType type,
  ) {
    final avgX = readings.map((r) => r.x).reduce((a, b) => a + b) / readings.length;
    final avgY = readings.map((r) => r.y).reduce((a, b) => a + b) / readings.length;
    final avgZ = readings.map((r) => r.z).reduce((a, b) => a + b) / readings.length;

    return SensorReading(
      x: avgX,
      y: avgY,
      z: avgZ,
      timestamp: DateTime.now(),
      type: type,
    );
  }

  /// Detect motion from sensor readings
  void _detectMotion(
    SensorReading current,
    SensorReading previous,
    SensorReading baseline,
  ) {
    // Calculate change from baseline
    final changeFromBaseline = current.differenceFrom(baseline);

    // Calculate rate of change
    final rateOfChange = current.differenceFrom(previous);

    // Determine if motion exceeds thresholds
    final threshold = current.type == SensorType.accelerometer
        ? sensitivity.accelerometerThreshold
        : sensitivity.gyroscopeThreshold;

    if (changeFromBaseline > threshold || rateOfChange > threshold * 0.5) {
      // Calculate intensity (0.0 to 1.0)
      final intensity = min(
        1.0,
        max(changeFromBaseline, rateOfChange) / (threshold * 2),
      );

      // Determine motion type
      final motionType = _classifyMotion(
        changeFromBaseline,
        rateOfChange,
        current.type,
      );

      final event = MotionEvent(
        intensity: intensity,
        type: motionType,
        timestamp: current.timestamp,
        triggerReading: current,
      );

      _motionEventController.add(event);
    }
  }

  /// Classify the type of motion detected
  MotionType _classifyMotion(
    double changeFromBaseline,
    double rateOfChange,
    SensorType sensorType,
  ) {
    // High rate of change = impact or shake
    if (rateOfChange > changeFromBaseline * 0.8) {
      if (sensorType == SensorType.accelerometer) {
        return rateOfChange > sensitivity.accelerometerThreshold * 2
            ? MotionType.impact
            : MotionType.shake;
      } else {
        return MotionType.shake;
      }
    }

    // Gradual change = tilt
    if (changeFromBaseline > sensitivity.accelerometerThreshold * 0.5) {
      return MotionType.tilt;
    }

    // Small movements
    return MotionType.subtle;
  }

  /// Manually recalibrate sensors (useful after car parks in new position)
  Future<void> recalibrate() async {
    _resetCalibration();
  }
}

/// Provider for sensor detection service
final sensorDetectionServiceProvider = Provider.family<SensorDetectionService, AlarmSensitivity>(
  (ref, sensitivity) => SensorDetectionService(sensitivity: sensitivity),
);

/// Provider for current alarm sensitivity
final alarmSensitivityProvider = Provider<AlarmSensitivity>((ref) {
  final settings = ref.watch(appSettingsProvider);
  final service = ref.watch(appSettingsServiceProvider);
  return service.getAlarmSensitivity(settings);
});

/// Provider for monitoring state
final sensorMonitoringProvider = StateProvider<bool>((ref) => false);
