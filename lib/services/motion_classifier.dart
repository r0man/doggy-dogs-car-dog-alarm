import '../models/sensor_data.dart';
import 'motion_analyzer.dart';

/// Pure logic class for classifying detected motion into motion types
/// This class is platform-independent and fully testable
class MotionClassifier {
  final AlarmSensitivity sensitivity;

  const MotionClassifier({required this.sensitivity});

  /// Classify the type of motion based on analysis results
  MotionType classifyMotion(MotionAnalysisResult analysis) {
    final changeFromBaseline = analysis.changeFromBaseline;
    final rateOfChange = analysis.rateOfChange;
    final sensorType = analysis.sensorType;
    final threshold = analysis.threshold;

    // High rate of change = impact or shake
    if (rateOfChange > changeFromBaseline * 0.8) {
      if (sensorType == SensorType.accelerometer) {
        return rateOfChange > threshold * 2
            ? MotionType.impact
            : MotionType.shake;
      } else {
        return MotionType.shake;
      }
    }

    // Gradual change = tilt
    if (changeFromBaseline > threshold * 0.5) {
      return MotionType.tilt;
    }

    // Small movements
    return MotionType.subtle;
  }

  /// Determine the severity level of motion (1-5 scale)
  int getMotionSeverity(MotionType type, double intensity) {
    // Base severity from motion type
    int baseSeverity;
    switch (type) {
      case MotionType.impact:
        baseSeverity = 5;
        break;
      case MotionType.shake:
        baseSeverity = 4;
        break;
      case MotionType.tilt:
        baseSeverity = 3;
        break;
      case MotionType.subtle:
        baseSeverity = 1;
        break;
    }

    // Adjust by intensity
    if (intensity >= 0.8) {
      return baseSeverity;
    } else if (intensity >= 0.5) {
      return (baseSeverity - 1).clamp(1, 5);
    } else {
      return (baseSeverity - 2).clamp(1, 5);
    }
  }

  /// Check if motion type is considered dangerous
  bool isDangerousMotion(MotionType type) {
    return type == MotionType.impact || type == MotionType.shake;
  }

  /// Get a human-readable description of the motion
  String describeMotion(MotionType type, double intensity) {
    final intensityDesc = _getIntensityDescription(intensity);
    final typeDesc = _getMotionTypeDescription(type);
    return '$intensityDesc $typeDesc';
  }

  String _getIntensityDescription(double intensity) {
    if (intensity >= 0.8) return 'severe';
    if (intensity >= 0.6) return 'strong';
    if (intensity >= 0.4) return 'moderate';
    if (intensity >= 0.2) return 'light';
    return 'very light';
  }

  String _getMotionTypeDescription(MotionType type) {
    switch (type) {
      case MotionType.impact:
        return 'impact';
      case MotionType.shake:
        return 'shaking';
      case MotionType.tilt:
        return 'tilting';
      case MotionType.subtle:
        return 'movement';
    }
  }
}
