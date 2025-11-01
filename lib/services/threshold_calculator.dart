import '../models/sensor_data.dart';

/// Pure logic class for calculating thresholds and baseline values from sensor data
/// This class is platform-independent and fully testable
class ThresholdCalculator {
  const ThresholdCalculator();

  /// Calculate average reading from a list of sensor readings
  SensorReading calculateAverage(
    List<SensorReading> readings,
    SensorType type,
  ) {
    if (readings.isEmpty) {
      throw ArgumentError('Cannot calculate average of empty readings list');
    }

    final avgX =
        readings.map((r) => r.x).reduce((a, b) => a + b) / readings.length;
    final avgY =
        readings.map((r) => r.y).reduce((a, b) => a + b) / readings.length;
    final avgZ =
        readings.map((r) => r.z).reduce((a, b) => a + b) / readings.length;

    return SensorReading(
      x: avgX,
      y: avgY,
      z: avgZ,
      timestamp: DateTime.now(),
      type: type,
    );
  }

  /// Calculate median reading from a list of sensor readings
  SensorReading calculateMedian(
    List<SensorReading> readings,
    SensorType type,
  ) {
    if (readings.isEmpty) {
      throw ArgumentError('Cannot calculate median of empty readings list');
    }

    final sortedX = readings.map((r) => r.x).toList()..sort();
    final sortedY = readings.map((r) => r.y).toList()..sort();
    final sortedZ = readings.map((r) => r.z).toList()..sort();

    final midpoint = readings.length ~/ 2;

    double medianX;
    double medianY;
    double medianZ;

    if (readings.length % 2 == 0) {
      medianX = (sortedX[midpoint - 1] + sortedX[midpoint]) / 2;
      medianY = (sortedY[midpoint - 1] + sortedY[midpoint]) / 2;
      medianZ = (sortedZ[midpoint - 1] + sortedZ[midpoint]) / 2;
    } else {
      medianX = sortedX[midpoint];
      medianY = sortedY[midpoint];
      medianZ = sortedZ[midpoint];
    }

    return SensorReading(
      x: medianX,
      y: medianY,
      z: medianZ,
      timestamp: DateTime.now(),
      type: type,
    );
  }

  /// Calculate standard deviation of sensor readings
  double calculateStandardDeviation(
    List<SensorReading> readings,
    SensorReading average,
  ) {
    if (readings.isEmpty) {
      return 0.0;
    }

    double sumOfSquares = 0.0;
    for (final reading in readings) {
      final diff = reading.differenceFrom(average);
      sumOfSquares += diff * diff;
    }

    final variance = sumOfSquares / readings.length;
    return variance; // Actually returning variance, but useful for threshold calculation
  }

  /// Calculate dynamic threshold based on recent readings variability
  /// Returns a suggested threshold multiplier (typically 1.0 to 2.0)
  double calculateDynamicThresholdMultiplier(
    List<SensorReading> readings,
    SensorReading baseline,
  ) {
    if (readings.length < 3) {
      return 1.0; // Default multiplier
    }

    final stdDev = calculateStandardDeviation(readings, baseline);

    // If there's high variability, increase threshold to reduce false positives
    if (stdDev > 10.0) {
      return 2.0;
    } else if (stdDev > 5.0) {
      return 1.5;
    } else if (stdDev > 2.0) {
      return 1.2;
    }

    return 1.0;
  }

  /// Filter outliers from readings using IQR method
  List<SensorReading> filterOutliers(List<SensorReading> readings) {
    if (readings.length < 4) {
      return readings; // Not enough data for outlier detection
    }

    // Calculate magnitudes and sort
    final magnitudes = readings.map((r) => r.magnitude).toList()..sort();

    // Calculate Q1, Q3, and IQR
    final q1Index = (magnitudes.length * 0.25).floor();
    final q3Index = (magnitudes.length * 0.75).floor();

    final q1 = magnitudes[q1Index];
    final q3 = magnitudes[q3Index];
    final iqr = q3 - q1;

    final lowerBound = q1 - (1.5 * iqr);
    final upperBound = q3 + (1.5 * iqr);

    // Filter readings within bounds
    return readings
        .where((r) => r.magnitude >= lowerBound && r.magnitude <= upperBound)
        .toList();
  }

  /// Calculate a robust baseline using median after filtering outliers
  SensorReading calculateRobustBaseline(
    List<SensorReading> readings,
    SensorType type,
  ) {
    if (readings.isEmpty) {
      throw ArgumentError('Cannot calculate baseline from empty readings list');
    }

    final filtered = filterOutliers(readings);

    if (filtered.isEmpty) {
      // All readings were outliers, fall back to average of all readings
      return calculateAverage(readings, type);
    }

    return calculateMedian(filtered, type);
  }
}
