import 'package:flutter_test/flutter_test.dart';
import 'package:doggy_dogs_car_alarm/models/sensor_data.dart';
import 'package:doggy_dogs_car_alarm/services/motion_analyzer.dart';
import 'package:doggy_dogs_car_alarm/services/motion_classifier.dart';

void main() {
  group('MotionClassifier - Classification Logic', () {
    test('classifies high rate of change as shake for accelerometer', () {
      const classifier = MotionClassifier(sensitivity: AlarmSensitivity.medium);

      const analysis = MotionAnalysisResult(
        changeFromBaseline: 5.0,
        rateOfChange: 4.5,
        intensity: 0.75,
        threshold: 3.0,
        sensorType: SensorType.accelerometer,
      );

      final type = classifier.classifyMotion(analysis);

      expect(type, MotionType.shake);
    });

    test('classifies very high rate of change as impact', () {
      const classifier = MotionClassifier(sensitivity: AlarmSensitivity.medium);

      const analysis = MotionAnalysisResult(
        changeFromBaseline: 10.0,
        rateOfChange: 9.0,
        intensity: 1.0,
        threshold: 3.0,
        sensorType: SensorType.accelerometer,
      );

      final type = classifier.classifyMotion(analysis);

      // Rate > threshold * 2 (3.0 * 2 = 6.0) and rate > baseline * 0.8
      expect(type, MotionType.impact);
    });

    test('classifies gradual change as tilt', () {
      const classifier = MotionClassifier(sensitivity: AlarmSensitivity.medium);

      const analysis = MotionAnalysisResult(
        changeFromBaseline: 3.0,
        rateOfChange: 0.5,
        intensity: 0.4,
        threshold: 3.0,
        sensorType: SensorType.accelerometer,
      );

      final type = classifier.classifyMotion(analysis);

      // Low rate of change, significant baseline change
      expect(type, MotionType.tilt);
    });

    test('classifies small movements as subtle', () {
      const classifier = MotionClassifier(sensitivity: AlarmSensitivity.high);

      const analysis = MotionAnalysisResult(
        changeFromBaseline: 0.5,
        rateOfChange: 0.3,
        intensity: 0.2,
        threshold: 1.5,
        sensorType: SensorType.accelerometer,
      );

      final type = classifier.classifyMotion(analysis);

      expect(type, MotionType.subtle);
    });

    test('gyroscope high rate of change classified as shake', () {
      const classifier = MotionClassifier(sensitivity: AlarmSensitivity.medium);

      const analysis = MotionAnalysisResult(
        changeFromBaseline: 3.0,
        rateOfChange: 2.5,
        intensity: 0.7,
        threshold: 1.5,
        sensorType: SensorType.gyroscope,
      );

      final type = classifier.classifyMotion(analysis);

      // Gyroscope never classifies as impact, only shake
      expect(type, MotionType.shake);
    });

    test('gyroscope never produces impact classification', () {
      const classifier = MotionClassifier(sensitivity: AlarmSensitivity.medium);

      const analysis = MotionAnalysisResult(
        changeFromBaseline: 20.0,
        rateOfChange: 18.0,
        intensity: 1.0,
        threshold: 1.5,
        sensorType: SensorType.gyroscope,
      );

      final type = classifier.classifyMotion(analysis);

      expect(type, isNot(MotionType.impact));
      expect(type, MotionType.shake);
    });
  });

  group('MotionClassifier - Motion Severity', () {
    test('impact with high intensity has maximum severity', () {
      const classifier = MotionClassifier(sensitivity: AlarmSensitivity.medium);

      final severity = classifier.getMotionSeverity(MotionType.impact, 0.9);

      expect(severity, equals(5));
    });

    test('shake with high intensity has severity 4', () {
      const classifier = MotionClassifier(sensitivity: AlarmSensitivity.medium);

      final severity = classifier.getMotionSeverity(MotionType.shake, 0.85);

      expect(severity, equals(4));
    });

    test('tilt with high intensity has severity 3', () {
      const classifier = MotionClassifier(sensitivity: AlarmSensitivity.medium);

      final severity = classifier.getMotionSeverity(MotionType.tilt, 0.8);

      expect(severity, equals(3));
    });

    test('subtle motion with any intensity has low severity', () {
      const classifier = MotionClassifier(sensitivity: AlarmSensitivity.medium);

      final severityHigh = classifier.getMotionSeverity(MotionType.subtle, 0.9);
      final severityLow = classifier.getMotionSeverity(MotionType.subtle, 0.2);

      expect(severityHigh, lessThanOrEqualTo(1));
      expect(severityLow, equals(1));
    });

    test('medium intensity reduces severity by 1', () {
      const classifier = MotionClassifier(sensitivity: AlarmSensitivity.medium);

      final severity = classifier.getMotionSeverity(MotionType.shake, 0.6);

      expect(severity, equals(3)); // 4 - 1
    });

    test('low intensity reduces severity by 2', () {
      const classifier = MotionClassifier(sensitivity: AlarmSensitivity.medium);

      final severity = classifier.getMotionSeverity(MotionType.shake, 0.4);

      expect(severity, equals(2)); // 4 - 2
    });

    test('severity is always between 1 and 5', () {
      const classifier = MotionClassifier(sensitivity: AlarmSensitivity.medium);

      for (final type in MotionType.values) {
        for (double intensity = 0.0; intensity <= 1.0; intensity += 0.1) {
          final severity = classifier.getMotionSeverity(type, intensity);
          expect(severity, greaterThanOrEqualTo(1));
          expect(severity, lessThanOrEqualTo(5));
        }
      }
    });
  });

  group('MotionClassifier - Dangerous Motion Detection', () {
    test('impact is considered dangerous', () {
      const classifier = MotionClassifier(sensitivity: AlarmSensitivity.medium);

      expect(classifier.isDangerousMotion(MotionType.impact), isTrue);
    });

    test('shake is considered dangerous', () {
      const classifier = MotionClassifier(sensitivity: AlarmSensitivity.medium);

      expect(classifier.isDangerousMotion(MotionType.shake), isTrue);
    });

    test('tilt is not considered dangerous', () {
      const classifier = MotionClassifier(sensitivity: AlarmSensitivity.medium);

      expect(classifier.isDangerousMotion(MotionType.tilt), isFalse);
    });

    test('subtle motion is not considered dangerous', () {
      const classifier = MotionClassifier(sensitivity: AlarmSensitivity.medium);

      expect(classifier.isDangerousMotion(MotionType.subtle), isFalse);
    });
  });

  group('MotionClassifier - Motion Description', () {
    test('describes severe impact correctly', () {
      const classifier = MotionClassifier(sensitivity: AlarmSensitivity.medium);

      final description = classifier.describeMotion(MotionType.impact, 0.9);

      expect(description, contains('severe'));
      expect(description, contains('impact'));
    });

    test('describes strong shaking correctly', () {
      const classifier = MotionClassifier(sensitivity: AlarmSensitivity.medium);

      final description = classifier.describeMotion(MotionType.shake, 0.7);

      expect(description, contains('strong'));
      expect(description, contains('shaking'));
    });

    test('describes moderate tilting correctly', () {
      const classifier = MotionClassifier(sensitivity: AlarmSensitivity.medium);

      final description = classifier.describeMotion(MotionType.tilt, 0.5);

      expect(description, contains('moderate'));
      expect(description, contains('tilting'));
    });

    test('describes light movement correctly', () {
      const classifier = MotionClassifier(sensitivity: AlarmSensitivity.medium);

      final description = classifier.describeMotion(MotionType.subtle, 0.3);

      expect(description, contains('light'));
      expect(description, contains('movement'));
    });

    test('describes very light movement correctly', () {
      const classifier = MotionClassifier(sensitivity: AlarmSensitivity.medium);

      final description = classifier.describeMotion(MotionType.subtle, 0.1);

      expect(description, contains('very light'));
      expect(description, contains('movement'));
    });

    test('intensity thresholds produce correct descriptions', () {
      const classifier = MotionClassifier(sensitivity: AlarmSensitivity.medium);

      expect(
        classifier.describeMotion(MotionType.shake, 0.8),
        contains('severe'),
      );
      expect(
        classifier.describeMotion(MotionType.shake, 0.6),
        contains('strong'),
      );
      expect(
        classifier.describeMotion(MotionType.shake, 0.4),
        contains('moderate'),
      );
      expect(
        classifier.describeMotion(MotionType.shake, 0.2),
        contains('light'),
      );
      expect(
        classifier.describeMotion(MotionType.shake, 0.1),
        contains('very light'),
      );
    });
  });

  group('MotionClassifier - Edge Cases', () {
    test('handles boundary between shake and tilt', () {
      const classifier = MotionClassifier(sensitivity: AlarmSensitivity.medium);

      // Above 0.8 ratio should be shake
      const analysisAbove = MotionAnalysisResult(
        changeFromBaseline: 5.0,
        rateOfChange: 4.1,
        intensity: 0.7,
        threshold: 3.0,
        sensorType: SensorType.accelerometer,
      );

      final typeAbove = classifier.classifyMotion(analysisAbove);
      expect(typeAbove, isNot(MotionType.tilt));

      // Just below 0.8 ratio should be tilt
      const analysisBelow = MotionAnalysisResult(
        changeFromBaseline: 5.0,
        rateOfChange: 3.9,
        intensity: 0.7,
        threshold: 3.0,
        sensorType: SensorType.accelerometer,
      );

      final typeBelow = classifier.classifyMotion(analysisBelow);
      expect(typeBelow, MotionType.tilt);
    });

    test('handles zero intensity', () {
      const classifier = MotionClassifier(sensitivity: AlarmSensitivity.medium);

      final severity = classifier.getMotionSeverity(MotionType.impact, 0.0);

      expect(severity, greaterThanOrEqualTo(1));
      expect(severity, lessThanOrEqualTo(5));
    });

    test('handles maximum intensity', () {
      const classifier = MotionClassifier(sensitivity: AlarmSensitivity.medium);

      final severity = classifier.getMotionSeverity(MotionType.impact, 1.0);

      expect(severity, equals(5));
    });

    test('handles all motion types with all sensitivities', () {
      for (final sensitivity in AlarmSensitivity.all) {
        final classifier = MotionClassifier(sensitivity: sensitivity);

        for (final type in MotionType.values) {
          // Should not throw for any combination
          expect(
            () => classifier.isDangerousMotion(type),
            returnsNormally,
          );
          expect(
            () => classifier.describeMotion(type, 0.5),
            returnsNormally,
          );
          expect(
            () => classifier.getMotionSeverity(type, 0.5),
            returnsNormally,
          );
        }
      }
    });
  });

  group('MotionClassifier - Comprehensive Scenarios', () {
    test('car bumped (moderate impact) scenario', () {
      const classifier = MotionClassifier(sensitivity: AlarmSensitivity.medium);

      // For impact: rateOfChange > changeFromBaseline * 0.8 AND rateOfChange > threshold * 2
      // threshold * 2 = 3.0 * 2 = 6.0
      // So rateOfChange must be > 6.0 AND > changeFromBaseline * 0.8
      const analysis = MotionAnalysisResult(
        changeFromBaseline: 7.0,
        rateOfChange: 6.5,
        intensity: 0.75,
        threshold: 3.0,
        sensorType: SensorType.accelerometer,
      );

      final type = classifier.classifyMotion(analysis);
      final severity = classifier.getMotionSeverity(type, analysis.intensity);
      final description = classifier.describeMotion(type, analysis.intensity);

      expect(type, MotionType.impact);
      expect(severity, greaterThanOrEqualTo(4));
      expect(classifier.isDangerousMotion(type), isTrue);
      expect(description, contains('strong'));
    });

    test('car door opened (gentle tilt) scenario', () {
      const classifier = MotionClassifier(sensitivity: AlarmSensitivity.medium);

      const analysis = MotionAnalysisResult(
        changeFromBaseline: 2.0,
        rateOfChange: 0.3,
        intensity: 0.35,
        threshold: 3.0,
        sensorType: SensorType.accelerometer,
      );

      final type = classifier.classifyMotion(analysis);
      final severity = classifier.getMotionSeverity(type, analysis.intensity);
      final description = classifier.describeMotion(type, analysis.intensity);

      expect(type, MotionType.tilt);
      expect(severity, lessThanOrEqualTo(2));
      expect(classifier.isDangerousMotion(type), isFalse);
      expect(description, contains('light'));
    });

    test('car shaken by wind (oscillating shake) scenario', () {
      const classifier = MotionClassifier(sensitivity: AlarmSensitivity.high);

      const analysis = MotionAnalysisResult(
        changeFromBaseline: 2.5,
        rateOfChange: 2.2,
        intensity: 0.6,
        threshold: 1.5,
        sensorType: SensorType.accelerometer,
      );

      final type = classifier.classifyMotion(analysis);
      final severity = classifier.getMotionSeverity(type, analysis.intensity);

      expect(type, MotionType.shake);
      expect(severity, greaterThanOrEqualTo(3));
      expect(classifier.isDangerousMotion(type), isTrue);
    });

    test('gyroscope rotation detection scenario', () {
      const classifier = MotionClassifier(sensitivity: AlarmSensitivity.medium);

      const analysis = MotionAnalysisResult(
        changeFromBaseline: 3.0,
        rateOfChange: 2.8,
        intensity: 0.8,
        threshold: 1.5,
        sensorType: SensorType.gyroscope,
      );

      final type = classifier.classifyMotion(analysis);

      expect(type, MotionType.shake);
      expect(classifier.isDangerousMotion(type), isTrue);
    });
  });

  group('MotionClassifier - Sensitivity Independence', () {
    test('classification uses threshold value from analysis result', () {
      const classifier = MotionClassifier(sensitivity: AlarmSensitivity.medium);

      // Test that classification is based on the threshold value in the analysis
      const shakeAnalysis = MotionAnalysisResult(
        changeFromBaseline: 5.0,
        rateOfChange: 4.5,
        intensity: 0.75,
        threshold: 5.0, // rateOfChange 4.5 < threshold * 2 (10.0) so shake
        sensorType: SensorType.accelerometer,
      );

      final shakeType = classifier.classifyMotion(shakeAnalysis);
      expect(shakeType, MotionType.shake);

      // Same values but lower threshold results in impact
      const impactAnalysis = MotionAnalysisResult(
        changeFromBaseline: 5.0,
        rateOfChange: 4.5,
        intensity: 0.75,
        threshold: 2.0, // rateOfChange 4.5 > threshold * 2 (4.0) so impact
        sensorType: SensorType.accelerometer,
      );

      final impactType = classifier.classifyMotion(impactAnalysis);
      expect(impactType, MotionType.impact);
    });

    test('descriptions are independent of sensitivity', () {
      const lowClassifier = MotionClassifier(sensitivity: AlarmSensitivity.low);
      const highClassifier =
          MotionClassifier(sensitivity: AlarmSensitivity.high);

      final desc1 = lowClassifier.describeMotion(MotionType.impact, 0.8);
      final desc2 = highClassifier.describeMotion(MotionType.impact, 0.8);

      expect(desc1, equals(desc2));
    });
  });
}
