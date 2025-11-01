import 'dart:math';
import '../models/sensor_data.dart';

/// Pure logic class for analyzing motion from sensor readings
/// This class is platform-independent and fully testable with mock data
class MotionAnalyzer {
  final AlarmSensitivity sensitivity;

  const MotionAnalyzer({required this.sensitivity});

  /// Analyze motion between current and previous readings relative to baseline
  /// Returns null if no significant motion detected, otherwise returns MotionAnalysisResult
  MotionAnalysisResult? analyzeMotion({
    required SensorReading current,
    required SensorReading previous,
    required SensorReading baseline,
  }) {
    // Calculate change from baseline
    final changeFromBaseline = current.differenceFrom(baseline);

    // Calculate rate of change
    final rateOfChange = current.differenceFrom(previous);

    // Determine if motion exceeds thresholds
    final threshold = _getThresholdForSensorType(current.type);

    if (changeFromBaseline > threshold || rateOfChange > threshold * 0.5) {
      // Calculate intensity (0.0 to 1.0)
      final intensity = min(
        1.0,
        max(changeFromBaseline, rateOfChange) / (threshold * 2),
      );

      return MotionAnalysisResult(
        changeFromBaseline: changeFromBaseline,
        rateOfChange: rateOfChange,
        intensity: intensity,
        threshold: threshold,
        sensorType: current.type,
      );
    }

    return null;
  }

  /// Get the threshold value for a specific sensor type
  double _getThresholdForSensorType(SensorType type) {
    return type == SensorType.accelerometer
        ? sensitivity.accelerometerThreshold
        : sensitivity.gyroscopeThreshold;
  }

  /// Check if a reading is within calibration tolerance
  bool isWithinCalibrationTolerance({
    required SensorReading reading,
    required SensorReading baseline,
    double toleranceMultiplier = 0.1,
  }) {
    final threshold = _getThresholdForSensorType(reading.type);
    final difference = reading.differenceFrom(baseline);
    return difference <= threshold * toleranceMultiplier;
  }

  /// Calculate motion stability score (0.0 = highly unstable, 1.0 = perfectly stable)
  double calculateStabilityScore({
    required List<SensorReading> readings,
    required SensorReading baseline,
  }) {
    if (readings.isEmpty) return 1.0;

    final threshold = _getThresholdForSensorType(readings.first.type);
    double totalDeviation = 0.0;

    for (final reading in readings) {
      totalDeviation += reading.differenceFrom(baseline);
    }

    final avgDeviation = totalDeviation / readings.length;
    final stabilityScore = max(0.0, 1.0 - (avgDeviation / threshold));
    return stabilityScore;
  }
}

/// Result of motion analysis containing all calculated values
class MotionAnalysisResult {
  final double changeFromBaseline;
  final double rateOfChange;
  final double intensity;
  final double threshold;
  final SensorType sensorType;

  const MotionAnalysisResult({
    required this.changeFromBaseline,
    required this.rateOfChange,
    required this.intensity,
    required this.threshold,
    required this.sensorType,
  });

  @override
  String toString() =>
      'MotionAnalysisResult(intensity=$intensity, changeFromBaseline=$changeFromBaseline, rateOfChange=$rateOfChange)';
}
