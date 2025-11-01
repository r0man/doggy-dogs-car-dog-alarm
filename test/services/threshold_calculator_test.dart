import 'package:flutter_test/flutter_test.dart';
import 'package:doggy_dogs_car_alarm/models/sensor_data.dart';
import 'package:doggy_dogs_car_alarm/services/threshold_calculator.dart';

void main() {
  group('ThresholdCalculator - Average Calculation', () {
    test('calculates average of identical readings', () {
      const calculator = ThresholdCalculator();

      final readings = List.generate(
        5,
        (i) => SensorReading(
          x: 1.0,
          y: 2.0,
          z: 3.0,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
      );

      final average =
          calculator.calculateAverage(readings, SensorType.accelerometer);

      expect(average.x, 1.0);
      expect(average.y, 2.0);
      expect(average.z, 3.0);
      expect(average.type, SensorType.accelerometer);
    });

    test('calculates average of varying readings', () {
      const calculator = ThresholdCalculator();

      final readings = [
        SensorReading(
          x: 0.0,
          y: 0.0,
          z: 0.0,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
        SensorReading(
          x: 2.0,
          y: 4.0,
          z: 6.0,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
        SensorReading(
          x: 4.0,
          y: 8.0,
          z: 12.0,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
      ];

      final average =
          calculator.calculateAverage(readings, SensorType.accelerometer);

      expect(average.x, closeTo(2.0, 0.01));
      expect(average.y, closeTo(4.0, 0.01));
      expect(average.z, closeTo(6.0, 0.01));
    });

    test('calculates average with negative values', () {
      const calculator = ThresholdCalculator();

      final readings = [
        SensorReading(
          x: -1.0,
          y: -2.0,
          z: -3.0,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
        SensorReading(
          x: 1.0,
          y: 2.0,
          z: 3.0,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
      ];

      final average =
          calculator.calculateAverage(readings, SensorType.accelerometer);

      expect(average.x, closeTo(0.0, 0.01));
      expect(average.y, closeTo(0.0, 0.01));
      expect(average.z, closeTo(0.0, 0.01));
    });

    test('throws error for empty readings list', () {
      const calculator = ThresholdCalculator();

      expect(
        () => calculator.calculateAverage([], SensorType.accelerometer),
        throwsArgumentError,
      );
    });

    test('handles single reading', () {
      const calculator = ThresholdCalculator();

      final readings = [
        SensorReading(
          x: 5.5,
          y: 6.6,
          z: 7.7,
          timestamp: DateTime.now(),
          type: SensorType.gyroscope,
        ),
      ];

      final average =
          calculator.calculateAverage(readings, SensorType.gyroscope);

      expect(average.x, 5.5);
      expect(average.y, 6.6);
      expect(average.z, 7.7);
      expect(average.type, SensorType.gyroscope);
    });
  });

  group('ThresholdCalculator - Median Calculation', () {
    test('calculates median of odd number of readings', () {
      const calculator = ThresholdCalculator();

      final readings = [
        SensorReading(
          x: 1.0,
          y: 1.0,
          z: 1.0,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
        SensorReading(
          x: 2.0,
          y: 2.0,
          z: 2.0,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
        SensorReading(
          x: 3.0,
          y: 3.0,
          z: 3.0,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
      ];

      final median =
          calculator.calculateMedian(readings, SensorType.accelerometer);

      expect(median.x, 2.0);
      expect(median.y, 2.0);
      expect(median.z, 2.0);
    });

    test('calculates median of even number of readings', () {
      const calculator = ThresholdCalculator();

      final readings = [
        SensorReading(
          x: 1.0,
          y: 1.0,
          z: 1.0,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
        SensorReading(
          x: 2.0,
          y: 2.0,
          z: 2.0,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
        SensorReading(
          x: 3.0,
          y: 3.0,
          z: 3.0,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
        SensorReading(
          x: 4.0,
          y: 4.0,
          z: 4.0,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
      ];

      final median =
          calculator.calculateMedian(readings, SensorType.accelerometer);

      expect(median.x, 2.5);
      expect(median.y, 2.5);
      expect(median.z, 2.5);
    });

    test('calculates median with outliers', () {
      const calculator = ThresholdCalculator();

      final readings = [
        SensorReading(
          x: 1.0,
          y: 1.0,
          z: 1.0,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
        SensorReading(
          x: 2.0,
          y: 2.0,
          z: 2.0,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
        SensorReading(
          x: 3.0,
          y: 3.0,
          z: 3.0,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
        SensorReading(
          x: 100.0,
          y: 100.0,
          z: 100.0,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
      ];

      final median =
          calculator.calculateMedian(readings, SensorType.accelerometer);

      // Median should be less affected by outlier than average
      expect(median.x, 2.5);
      expect(median.y, 2.5);
      expect(median.z, 2.5);
    });

    test('throws error for empty readings list', () {
      const calculator = ThresholdCalculator();

      expect(
        () => calculator.calculateMedian([], SensorType.accelerometer),
        throwsArgumentError,
      );
    });

    test('handles single reading', () {
      const calculator = ThresholdCalculator();

      final readings = [
        SensorReading(
          x: 7.5,
          y: 8.5,
          z: 9.5,
          timestamp: DateTime.now(),
          type: SensorType.gyroscope,
        ),
      ];

      final median = calculator.calculateMedian(readings, SensorType.gyroscope);

      expect(median.x, 7.5);
      expect(median.y, 8.5);
      expect(median.z, 9.5);
    });
  });

  group('ThresholdCalculator - Standard Deviation', () {
    test('calculates zero deviation for identical readings', () {
      const calculator = ThresholdCalculator();

      final baseline = SensorReading(
        x: 5.0,
        y: 5.0,
        z: 5.0,
        timestamp: DateTime.now(),
        type: SensorType.accelerometer,
      );

      final readings = List.generate(
        5,
        (i) => baseline.copyWith(),
      );

      final stdDev = calculator.calculateStandardDeviation(readings, baseline);

      expect(stdDev, 0.0);
    });

    test('calculates non-zero deviation for varying readings', () {
      const calculator = ThresholdCalculator();

      final baseline = SensorReading(
        x: 0.0,
        y: 0.0,
        z: 0.0,
        timestamp: DateTime.now(),
        type: SensorType.accelerometer,
      );

      final readings = [
        SensorReading(
          x: 1.0,
          y: 0.0,
          z: 0.0,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
        SensorReading(
          x: -1.0,
          y: 0.0,
          z: 0.0,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
        SensorReading(
          x: 2.0,
          y: 0.0,
          z: 0.0,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
        SensorReading(
          x: -2.0,
          y: 0.0,
          z: 0.0,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
      ];

      final stdDev = calculator.calculateStandardDeviation(readings, baseline);

      expect(stdDev, greaterThan(0.0));
    });

    test('returns zero for empty readings', () {
      const calculator = ThresholdCalculator();

      final baseline = SensorReading(
        x: 0.0,
        y: 0.0,
        z: 0.0,
        timestamp: DateTime.now(),
        type: SensorType.accelerometer,
      );

      final stdDev = calculator.calculateStandardDeviation([], baseline);

      expect(stdDev, 0.0);
    });
  });

  group('ThresholdCalculator - Dynamic Threshold Multiplier', () {
    test('returns 1.0 for stable readings', () {
      const calculator = ThresholdCalculator();

      final baseline = SensorReading(
        x: 0.0,
        y: 9.8,
        z: 0.0,
        timestamp: DateTime.now(),
        type: SensorType.accelerometer,
      );

      final readings = List.generate(
        10,
        (i) => SensorReading(
          x: 0.1,
          y: 9.9,
          z: 0.1,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
      );

      final multiplier = calculator.calculateDynamicThresholdMultiplier(
        readings,
        baseline,
      );

      expect(multiplier, 1.0);
    });

    test('returns higher multiplier for unstable readings', () {
      const calculator = ThresholdCalculator();

      final baseline = SensorReading(
        x: 0.0,
        y: 9.8,
        z: 0.0,
        timestamp: DateTime.now(),
        type: SensorType.accelerometer,
      );

      final readings = [
        SensorReading(
          x: 5.0,
          y: 9.8,
          z: 0.0,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
        SensorReading(
          x: -5.0,
          y: 9.8,
          z: 5.0,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
        SensorReading(
          x: 3.0,
          y: 15.0,
          z: -3.0,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
        SensorReading(
          x: -4.0,
          y: 5.0,
          z: 4.0,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
      ];

      final multiplier = calculator.calculateDynamicThresholdMultiplier(
        readings,
        baseline,
      );

      expect(multiplier, greaterThan(1.0));
    });

    test('returns 1.0 for insufficient data', () {
      const calculator = ThresholdCalculator();

      final baseline = SensorReading(
        x: 0.0,
        y: 9.8,
        z: 0.0,
        timestamp: DateTime.now(),
        type: SensorType.accelerometer,
      );

      final readings = [
        SensorReading(
          x: 0.0,
          y: 9.8,
          z: 0.0,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
        SensorReading(
          x: 1.0,
          y: 9.8,
          z: 0.0,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
      ];

      final multiplier = calculator.calculateDynamicThresholdMultiplier(
        readings,
        baseline,
      );

      expect(multiplier, 1.0);
    });

    test('multiplier scales with deviation level', () {
      const calculator = ThresholdCalculator();

      final baseline = SensorReading(
        x: 0.0,
        y: 9.8,
        z: 0.0,
        timestamp: DateTime.now(),
        type: SensorType.accelerometer,
      );

      // Low deviation
      final stableReadings = List.generate(
        5,
        (i) => SensorReading(
          x: 0.5,
          y: 10.0,
          z: 0.5,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
      );

      final lowMultiplier = calculator.calculateDynamicThresholdMultiplier(
        stableReadings,
        baseline,
      );

      // High deviation
      final unstableReadings = List.generate(
        5,
        (i) => SensorReading(
          x: i * 5.0,
          y: 9.8 + i * 3.0,
          z: i * 4.0,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
      );

      final highMultiplier = calculator.calculateDynamicThresholdMultiplier(
        unstableReadings,
        baseline,
      );

      expect(highMultiplier, greaterThan(lowMultiplier));
    });
  });

  group('ThresholdCalculator - Outlier Filtering', () {
    test('removes outliers from readings', () {
      const calculator = ThresholdCalculator();

      final readings = [
        SensorReading(
          x: 1.0,
          y: 9.8,
          z: 0.0,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
        SensorReading(
          x: 1.1,
          y: 9.9,
          z: 0.1,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
        SensorReading(
          x: 0.9,
          y: 9.7,
          z: -0.1,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
        SensorReading(
          x: 100.0,
          y: 100.0,
          z: 100.0,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
        SensorReading(
          x: 0.95,
          y: 9.75,
          z: 0.05,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
      ];

      final filtered = calculator.filterOutliers(readings);

      // Should filter out the outlier (100, 100, 100)
      expect(filtered.length, lessThan(readings.length));
      expect(filtered.length, greaterThanOrEqualTo(4));
    });

    test('keeps all readings when no outliers present', () {
      const calculator = ThresholdCalculator();

      final readings = List.generate(
        10,
        (i) => SensorReading(
          x: 1.0 + i * 0.1,
          y: 9.8 + i * 0.1,
          z: 0.0,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
      );

      final filtered = calculator.filterOutliers(readings);

      expect(filtered.length, readings.length);
    });

    test('returns all readings when not enough data', () {
      const calculator = ThresholdCalculator();

      final readings = [
        SensorReading(
          x: 1.0,
          y: 9.8,
          z: 0.0,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
        SensorReading(
          x: 100.0,
          y: 100.0,
          z: 100.0,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
      ];

      final filtered = calculator.filterOutliers(readings);

      expect(filtered.length, readings.length);
    });

    test('handles empty readings list', () {
      const calculator = ThresholdCalculator();

      final filtered = calculator.filterOutliers([]);

      expect(filtered, isEmpty);
    });
  });

  group('ThresholdCalculator - Robust Baseline', () {
    test('calculates robust baseline using median after filtering', () {
      const calculator = ThresholdCalculator();

      final readings = [
        SensorReading(
          x: 0.0,
          y: 9.8,
          z: 0.0,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
        SensorReading(
          x: 0.1,
          y: 9.9,
          z: 0.1,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
        SensorReading(
          x: -0.1,
          y: 9.7,
          z: -0.1,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
        SensorReading(
          x: 0.05,
          y: 9.85,
          z: 0.05,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
        SensorReading(
          x: 100.0,
          y: 100.0,
          z: 100.0,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
      ];

      final baseline = calculator.calculateRobustBaseline(
        readings,
        SensorType.accelerometer,
      );

      // Should be close to the cluster, not influenced by outlier
      expect(baseline.x, lessThan(10.0));
      expect(baseline.y, closeTo(9.8, 1.0));
    });

    test('falls back to average when all readings are outliers', () {
      const calculator = ThresholdCalculator();

      final readings = [
        SensorReading(
          x: 0.0,
          y: 0.0,
          z: 0.0,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
        SensorReading(
          x: 100.0,
          y: 100.0,
          z: 100.0,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
        SensorReading(
          x: -100.0,
          y: -100.0,
          z: -100.0,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
      ];

      final baseline = calculator.calculateRobustBaseline(
        readings,
        SensorType.accelerometer,
      );

      // Should return average since all were considered outliers
      expect(baseline, isNotNull);
      expect(baseline.type, SensorType.accelerometer);
    });

    test('throws error for empty readings list', () {
      const calculator = ThresholdCalculator();

      expect(
        () => calculator.calculateRobustBaseline([], SensorType.accelerometer),
        throwsArgumentError,
      );
    });

    test('handles single reading', () {
      const calculator = ThresholdCalculator();

      final readings = [
        SensorReading(
          x: 1.0,
          y: 2.0,
          z: 3.0,
          timestamp: DateTime.now(),
          type: SensorType.gyroscope,
        ),
      ];

      final baseline = calculator.calculateRobustBaseline(
        readings,
        SensorType.gyroscope,
      );

      expect(baseline.x, 1.0);
      expect(baseline.y, 2.0);
      expect(baseline.z, 3.0);
    });
  });

  group('ThresholdCalculator - Realistic Scenarios', () {
    test('calibration with stable car scenario', () {
      const calculator = ThresholdCalculator();

      // Simulating 10 samples of a car at rest with minor sensor noise
      final readings = List.generate(
        10,
        (i) => SensorReading(
          x: 0.0 + (i % 3 - 1) * 0.05,
          y: 9.8 + (i % 3 - 1) * 0.05,
          z: 0.0 + (i % 3 - 1) * 0.05,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
      );

      final baseline = calculator.calculateRobustBaseline(
        readings,
        SensorType.accelerometer,
      );

      expect(baseline.x, closeTo(0.0, 0.1));
      expect(baseline.y, closeTo(9.8, 0.1));
      expect(baseline.z, closeTo(0.0, 0.1));
    });

    test('calibration with noisy environment scenario', () {
      const calculator = ThresholdCalculator();

      // Car on slightly uneven ground with wind
      final readings = [
        SensorReading(
            x: 0.2,
            y: 9.9,
            z: 0.1,
            timestamp: DateTime.now(),
            type: SensorType.accelerometer),
        SensorReading(
            x: -0.1,
            y: 9.7,
            z: -0.2,
            timestamp: DateTime.now(),
            type: SensorType.accelerometer),
        SensorReading(
            x: 0.3,
            y: 10.0,
            z: 0.0,
            timestamp: DateTime.now(),
            type: SensorType.accelerometer),
        SensorReading(
            x: 0.0,
            y: 9.8,
            z: 0.1,
            timestamp: DateTime.now(),
            type: SensorType.accelerometer),
        SensorReading(
            x: 15.0,
            y: 15.0,
            z: 15.0,
            timestamp: DateTime.now(),
            type: SensorType.accelerometer), // Spike
      ];

      final baseline = calculator.calculateRobustBaseline(
        readings,
        SensorType.accelerometer,
      );

      final multiplier = calculator.calculateDynamicThresholdMultiplier(
        readings,
        baseline,
      );

      // Should get reasonable baseline and increased multiplier due to noise
      expect(baseline.x, closeTo(0.0, 2.0));
      expect(multiplier, greaterThan(1.0));
    });

    test('comparing average vs median for outlier resistance', () {
      const calculator = ThresholdCalculator();

      final readings = [
        SensorReading(
            x: 1.0,
            y: 1.0,
            z: 1.0,
            timestamp: DateTime.now(),
            type: SensorType.accelerometer),
        SensorReading(
            x: 1.1,
            y: 1.1,
            z: 1.1,
            timestamp: DateTime.now(),
            type: SensorType.accelerometer),
        SensorReading(
            x: 0.9,
            y: 0.9,
            z: 0.9,
            timestamp: DateTime.now(),
            type: SensorType.accelerometer),
        SensorReading(
            x: 50.0,
            y: 50.0,
            z: 50.0,
            timestamp: DateTime.now(),
            type: SensorType.accelerometer),
      ];

      final average =
          calculator.calculateAverage(readings, SensorType.accelerometer);
      final median =
          calculator.calculateMedian(readings, SensorType.accelerometer);

      // Median should be much less affected by the outlier
      expect(median.x, lessThan(average.x));
      expect(median.x, closeTo(1.0, 0.5));
    });
  });

  group('ThresholdCalculator - Edge Cases', () {
    test('handles very large values', () {
      const calculator = ThresholdCalculator();

      final readings = [
        SensorReading(
          x: 1000.0,
          y: 1000.0,
          z: 1000.0,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
        SensorReading(
          x: 2000.0,
          y: 2000.0,
          z: 2000.0,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
      ];

      final average =
          calculator.calculateAverage(readings, SensorType.accelerometer);

      expect(average.x, 1500.0);
      expect(average.y, 1500.0);
      expect(average.z, 1500.0);
    });

    test('handles very small values', () {
      const calculator = ThresholdCalculator();

      final readings = [
        SensorReading(
          x: 0.0001,
          y: 0.0002,
          z: 0.0003,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
        SensorReading(
          x: 0.0002,
          y: 0.0003,
          z: 0.0004,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
      ];

      final average =
          calculator.calculateAverage(readings, SensorType.accelerometer);

      expect(average.x, closeTo(0.00015, 0.00001));
      expect(average.y, closeTo(0.00025, 0.00001));
      expect(average.z, closeTo(0.00035, 0.00001));
    });

    test('handles mixed positive and negative values', () {
      const calculator = ThresholdCalculator();

      final readings = [
        SensorReading(
            x: -10.0,
            y: -10.0,
            z: -10.0,
            timestamp: DateTime.now(),
            type: SensorType.gyroscope),
        SensorReading(
            x: 10.0,
            y: 10.0,
            z: 10.0,
            timestamp: DateTime.now(),
            type: SensorType.gyroscope),
        SensorReading(
            x: -5.0,
            y: -5.0,
            z: -5.0,
            timestamp: DateTime.now(),
            type: SensorType.gyroscope),
        SensorReading(
            x: 5.0,
            y: 5.0,
            z: 5.0,
            timestamp: DateTime.now(),
            type: SensorType.gyroscope),
      ];

      final average =
          calculator.calculateAverage(readings, SensorType.gyroscope);
      final median = calculator.calculateMedian(readings, SensorType.gyroscope);

      expect(average.x, 0.0);
      expect(average.y, 0.0);
      expect(average.z, 0.0);
      expect(median.x, 0.0);
      expect(median.y, 0.0);
      expect(median.z, 0.0);
    });
  });
}
