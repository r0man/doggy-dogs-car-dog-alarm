import 'dart:math';

/// Represents a reading from device sensors (accelerometer/gyroscope)
class SensorReading {
  final double x;
  final double y;
  final double z;
  final DateTime timestamp;
  final SensorType type;

  const SensorReading({
    required this.x,
    required this.y,
    required this.z,
    required this.timestamp,
    required this.type,
  });

  /// Calculate magnitude of the sensor vector
  double get magnitude => sqrt(x * x + y * y + z * z);

  /// Calculate difference from another reading
  double differenceFrom(SensorReading other) {
    final dx = x - other.x;
    final dy = y - other.y;
    final dz = z - other.z;
    return sqrt(dx * dx + dy * dy + dz * dz);
  }

  SensorReading copyWith({
    double? x,
    double? y,
    double? z,
    DateTime? timestamp,
    SensorType? type,
  }) {
    return SensorReading(
      x: x ?? this.x,
      y: y ?? this.y,
      z: z ?? this.z,
      timestamp: timestamp ?? this.timestamp,
      type: type ?? this.type,
    );
  }

  @override
  String toString() =>
      'SensorReading($type: x=$x, y=$y, z=$z, magnitude=$magnitude)';
}

enum SensorType {
  accelerometer,
  gyroscope,
}

/// Represents a motion event detected by sensors
class MotionEvent {
  final double intensity; // 0.0 to 1.0
  final MotionType type;
  final DateTime timestamp;
  final SensorReading triggerReading;

  const MotionEvent({
    required this.intensity,
    required this.type,
    required this.timestamp,
    required this.triggerReading,
  });

  /// Check if motion is significant enough to trigger alarm
  bool shouldTriggerAlarm(AlarmSensitivity sensitivity) {
    return intensity >= sensitivity.threshold;
  }

  @override
  String toString() => 'MotionEvent($type: intensity=$intensity at $timestamp)';
}

enum MotionType {
  shake,
  tilt,
  impact,
  subtle,
}

/// Alarm sensitivity configuration
class AlarmSensitivity {
  final String name;
  final double threshold; // 0.0 to 1.0
  final double accelerometerThreshold; // m/s^2
  final double gyroscopeThreshold; // rad/s

  const AlarmSensitivity({
    required this.name,
    required this.threshold,
    required this.accelerometerThreshold,
    required this.gyroscopeThreshold,
  });

  /// Predefined sensitivity levels
  static const low = AlarmSensitivity(
    name: 'Low',
    threshold: 0.7,
    accelerometerThreshold: 5.0,
    gyroscopeThreshold: 2.0,
  );

  static const medium = AlarmSensitivity(
    name: 'Medium',
    threshold: 0.5,
    accelerometerThreshold: 3.0,
    gyroscopeThreshold: 1.5,
  );

  static const high = AlarmSensitivity(
    name: 'High',
    threshold: 0.3,
    accelerometerThreshold: 1.5,
    gyroscopeThreshold: 1.0,
  );

  static const veryHigh = AlarmSensitivity(
    name: 'Very High',
    threshold: 0.15,
    accelerometerThreshold: 0.8,
    gyroscopeThreshold: 0.5,
  );

  static const all = [low, medium, high, veryHigh];

  AlarmSensitivity copyWith({
    String? name,
    double? threshold,
    double? accelerometerThreshold,
    double? gyroscopeThreshold,
  }) {
    return AlarmSensitivity(
      name: name ?? this.name,
      threshold: threshold ?? this.threshold,
      accelerometerThreshold:
          accelerometerThreshold ?? this.accelerometerThreshold,
      gyroscopeThreshold: gyroscopeThreshold ?? this.gyroscopeThreshold,
    );
  }
}
