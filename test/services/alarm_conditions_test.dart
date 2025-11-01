import 'package:flutter_test/flutter_test.dart';
import 'package:doggy_dogs_car_alarm/services/alarm_conditions.dart';
import 'package:doggy_dogs_car_alarm/models/alarm_state.dart';
import 'package:doggy_dogs_car_alarm/models/sensor_data.dart';
import 'package:doggy_dogs_car_alarm/models/dog.dart';

void main() {
  group('AlarmConditions Tests', () {
    late Dog testDog;
    late AlarmConditions conditions;

    setUp(() {
      final now = DateTime.now();
      testDog = Dog(
        id: 'test-dog',
        name: 'Max',
        breed: DogBreed.germanShepherd,
        stats: const DogStats(
          hunger: 80,
          happiness: 80,
          energy: 80,
          loyalty: 80,
        ),
        personality: const DogPersonality(
          protective: true,
          brave: true,
        ),
        createdAt: now,
        lastInteraction: now,
      );

      conditions = AlarmConditions(
        sensitivity: AlarmSensitivity.medium,
        guardDog: testDog,
      );
    });

    group('evaluateMotion', () {
      test('ignores motion below sensitivity threshold', () {
        final event = MotionEvent(
          intensity: 0.3, // Below medium threshold (0.5)
          type: MotionType.subtle,
          timestamp: DateTime.now(),
          triggerReading: SensorReading(
            x: 0.1,
            y: 0.1,
            z: 0.1,
            timestamp: DateTime.now(),
            type: SensorType.accelerometer,
          ),
        );

        final decision = conditions.evaluateMotion(event, AlarmMode.standard);

        expect(decision.shouldTrigger, false);
        expect(decision.reason, TriggerReason.belowThreshold);
      });

      test('triggers immediately in aggressive mode', () {
        final event = MotionEvent(
          intensity: 0.6, // Above medium threshold
          type: MotionType.shake,
          timestamp: DateTime.now(),
          triggerReading: SensorReading(
            x: 1.0,
            y: 1.0,
            z: 1.0,
            timestamp: DateTime.now(),
            type: SensorType.accelerometer,
          ),
        );

        final decision = conditions.evaluateMotion(event, AlarmMode.aggressive);

        expect(decision.shouldTrigger, true);
        expect(decision.reason, TriggerReason.aggressiveMode);
      });

      test('requires verification in standard mode for moderate intensity', () {
        final event = MotionEvent(
          intensity: 0.6, // Above threshold but below immediate threshold
          type: MotionType.tilt,
          timestamp: DateTime.now(),
          triggerReading: SensorReading(
            x: 0.5,
            y: 0.5,
            z: 0.5,
            timestamp: DateTime.now(),
            type: SensorType.accelerometer,
          ),
        );

        final decision = conditions.evaluateMotion(event, AlarmMode.standard);

        expect(decision.shouldTrigger, false);
        expect(decision.reason, TriggerReason.needsVerification);
      });

      test('requires verification in stealth mode for moderate intensity', () {
        final event = MotionEvent(
          intensity: 0.6,
          type: MotionType.tilt,
          timestamp: DateTime.now(),
          triggerReading: SensorReading(
            x: 0.5,
            y: 0.5,
            z: 0.5,
            timestamp: DateTime.now(),
            type: SensorType.accelerometer,
          ),
        );

        final decision = conditions.evaluateMotion(event, AlarmMode.stealth);

        expect(decision.shouldTrigger, false);
        expect(decision.reason, TriggerReason.needsVerification);
      });

      test('triggers immediately for very high intensity regardless of mode',
          () {
        // With dog effectiveness of 80%, we need intensity > 1.0 to get effective > 0.8
        // So let's use intensity of 1.0 (max) to ensure effective intensity > 0.8
        // With effective intensity of 0.8, it's not > 0.8, so won't trigger immediately
        // Let's use a more effective dog for this test
        final highEffectivenessDog = testDog.copyWith(
          stats: const DogStats(
            hunger: 100,
            happiness: 100,
            energy: 100,
            loyalty: 100,
          ),
        );

        final highEffectivenessConditions = AlarmConditions(
          sensitivity: AlarmSensitivity.medium,
          guardDog: highEffectivenessDog,
        );

        final event2 = MotionEvent(
          intensity: 0.9, // With 100% effectiveness, effective = 0.9
          type: MotionType.impact,
          timestamp: DateTime.now(),
          triggerReading: SensorReading(
            x: 2.0,
            y: 2.0,
            z: 2.0,
            timestamp: DateTime.now(),
            type: SensorType.accelerometer,
          ),
        );

        final standardDecision2 = highEffectivenessConditions.evaluateMotion(
          event2,
          AlarmMode.standard,
        );
        expect(standardDecision2.shouldTrigger, true);
        expect(standardDecision2.reason, TriggerReason.highIntensity);

        final stealthDecision = highEffectivenessConditions.evaluateMotion(
          event2,
          AlarmMode.stealth,
        );
        expect(stealthDecision.shouldTrigger, true);
        expect(stealthDecision.reason, TriggerReason.highIntensity);
      });

      test('applies dog effectiveness modifier', () {
        // Create a less effective dog (50% effectiveness)
        final lowEffectivenessDog = testDog.copyWith(
          stats: const DogStats(
            hunger: 50,
            happiness: 50,
            energy: 50,
            loyalty: 50,
          ),
        );

        final lowEffectivenessConditions = AlarmConditions(
          sensitivity: AlarmSensitivity.medium,
          guardDog: lowEffectivenessDog,
        );

        // Event with 0.9 intensity (above immediate threshold)
        final event = MotionEvent(
          intensity: 0.9,
          type: MotionType.impact,
          timestamp: DateTime.now(),
          triggerReading: SensorReading(
            x: 2.0,
            y: 2.0,
            z: 2.0,
            timestamp: DateTime.now(),
            type: SensorType.accelerometer,
          ),
        );

        // With 50% effectiveness, 0.9 * 0.5 = 0.45, below immediate threshold
        final decision = lowEffectivenessConditions.evaluateMotion(
          event,
          AlarmMode.standard,
        );

        // Should require verification instead of immediate trigger
        expect(decision.shouldTrigger, false);
        expect(decision.reason, TriggerReason.needsVerification);
      });

      test('effective intensity is calculated correctly', () {
        final event = MotionEvent(
          intensity: 0.6,
          type: MotionType.shake,
          timestamp: DateTime.now(),
          triggerReading: SensorReading(
            x: 1.0,
            y: 1.0,
            z: 1.0,
            timestamp: DateTime.now(),
            type: SensorType.accelerometer,
          ),
        );

        final decision = conditions.evaluateMotion(event, AlarmMode.aggressive);

        // effectiveness is calculated from dog stats (average of all stats / 100)
        // testDog has all stats at 80, so effectiveness = 80
        // effective intensity = 0.6 * 0.8 = 0.48
        expect(decision.effectiveIntensity, closeTo(0.48, 0.01));
      });
    });

    group('isVerifiedThreat', () {
      test('returns false when not enough motions', () {
        final motions = [
          MotionEvent(
            intensity: 0.6,
            type: MotionType.shake,
            timestamp: DateTime.now(),
            triggerReading: SensorReading(
              x: 1.0,
              y: 1.0,
              z: 1.0,
              timestamp: DateTime.now(),
              type: SensorType.accelerometer,
            ),
          ),
        ];

        expect(conditions.isVerifiedThreat(motions), false);
      });

      test('returns true when enough motions within window', () {
        final now = DateTime.now();
        final motions = [
          MotionEvent(
            intensity: 0.6,
            type: MotionType.shake,
            timestamp: now.subtract(const Duration(seconds: 1)),
            triggerReading: SensorReading(
              x: 1.0,
              y: 1.0,
              z: 1.0,
              timestamp: now,
              type: SensorType.accelerometer,
            ),
          ),
          MotionEvent(
            intensity: 0.6,
            type: MotionType.shake,
            timestamp: now,
            triggerReading: SensorReading(
              x: 1.0,
              y: 1.0,
              z: 1.0,
              timestamp: now,
              type: SensorType.accelerometer,
            ),
          ),
        ];

        expect(conditions.isVerifiedThreat(motions), true);
      });

      test('returns false when motions are outside verification window', () {
        final now = DateTime.now();
        final motions = [
          MotionEvent(
            intensity: 0.6,
            type: MotionType.shake,
            timestamp: now.subtract(const Duration(seconds: 5)),
            triggerReading: SensorReading(
              x: 1.0,
              y: 1.0,
              z: 1.0,
              timestamp: now,
              type: SensorType.accelerometer,
            ),
          ),
          MotionEvent(
            intensity: 0.6,
            type: MotionType.shake,
            timestamp: now.subtract(const Duration(seconds: 4)),
            triggerReading: SensorReading(
              x: 1.0,
              y: 1.0,
              z: 1.0,
              timestamp: now,
              type: SensorType.accelerometer,
            ),
          ),
        ];

        expect(conditions.isVerifiedThreat(motions), false);
      });

      test('returns true when at least 2 motions are within window', () {
        final now = DateTime.now();
        final motions = [
          MotionEvent(
            intensity: 0.6,
            type: MotionType.shake,
            timestamp: now.subtract(const Duration(seconds: 5)),
            triggerReading: SensorReading(
              x: 1.0,
              y: 1.0,
              z: 1.0,
              timestamp: now,
              type: SensorType.accelerometer,
            ),
          ),
          MotionEvent(
            intensity: 0.6,
            type: MotionType.shake,
            timestamp: now.subtract(const Duration(seconds: 2)),
            triggerReading: SensorReading(
              x: 1.0,
              y: 1.0,
              z: 1.0,
              timestamp: now,
              type: SensorType.accelerometer,
            ),
          ),
          MotionEvent(
            intensity: 0.6,
            type: MotionType.shake,
            timestamp: now,
            triggerReading: SensorReading(
              x: 1.0,
              y: 1.0,
              z: 1.0,
              timestamp: now,
              type: SensorType.accelerometer,
            ),
          ),
        ];

        expect(conditions.isVerifiedThreat(motions), true);
      });
    });

    group('filterRecentMotions', () {
      test('filters out motions outside verification window', () {
        final now = DateTime.now();
        final motions = [
          MotionEvent(
            intensity: 0.6,
            type: MotionType.shake,
            timestamp: now.subtract(const Duration(seconds: 5)),
            triggerReading: SensorReading(
              x: 1.0,
              y: 1.0,
              z: 1.0,
              timestamp: now,
              type: SensorType.accelerometer,
            ),
          ),
          MotionEvent(
            intensity: 0.6,
            type: MotionType.shake,
            timestamp: now.subtract(const Duration(seconds: 2)),
            triggerReading: SensorReading(
              x: 1.0,
              y: 1.0,
              z: 1.0,
              timestamp: now,
              type: SensorType.accelerometer,
            ),
          ),
          MotionEvent(
            intensity: 0.6,
            type: MotionType.shake,
            timestamp: now,
            triggerReading: SensorReading(
              x: 1.0,
              y: 1.0,
              z: 1.0,
              timestamp: now,
              type: SensorType.accelerometer,
            ),
          ),
        ];

        final filtered = conditions.filterRecentMotions(motions);

        expect(filtered.length, 2); // Only the last 2 within 3-second window
      });

      test('keeps all motions when within window', () {
        final now = DateTime.now();
        final motions = [
          MotionEvent(
            intensity: 0.6,
            type: MotionType.shake,
            timestamp: now.subtract(const Duration(seconds: 2)),
            triggerReading: SensorReading(
              x: 1.0,
              y: 1.0,
              z: 1.0,
              timestamp: now,
              type: SensorType.accelerometer,
            ),
          ),
          MotionEvent(
            intensity: 0.6,
            type: MotionType.shake,
            timestamp: now.subtract(const Duration(seconds: 1)),
            triggerReading: SensorReading(
              x: 1.0,
              y: 1.0,
              z: 1.0,
              timestamp: now,
              type: SensorType.accelerometer,
            ),
          ),
        ];

        final filtered = conditions.filterRecentMotions(motions);

        expect(filtered.length, 2);
      });

      test('returns empty list when all motions are old', () {
        final now = DateTime.now();
        final motions = [
          MotionEvent(
            intensity: 0.6,
            type: MotionType.shake,
            timestamp: now.subtract(const Duration(seconds: 10)),
            triggerReading: SensorReading(
              x: 1.0,
              y: 1.0,
              z: 1.0,
              timestamp: now,
              type: SensorType.accelerometer,
            ),
          ),
        ];

        final filtered = conditions.filterRecentMotions(motions);

        expect(filtered.isEmpty, true);
      });
    });

    group('copyWith', () {
      test('creates new instance with updated sensitivity', () {
        final newConditions = conditions.copyWith(
          sensitivity: AlarmSensitivity.high,
        );

        expect(newConditions.sensitivity.name, 'High');
        expect(newConditions.guardDog, testDog); // Unchanged
      });

      test('creates new instance with updated dog', () {
        final newDog = testDog.copyWith(name: 'Rex');
        final newConditions = conditions.copyWith(guardDog: newDog);

        expect(newConditions.guardDog.name, 'Rex');
        expect(newConditions.sensitivity, AlarmSensitivity.medium); // Unchanged
      });

      test('creates new instance with both updated', () {
        final newDog = testDog.copyWith(name: 'Rex');
        final newConditions = conditions.copyWith(
          sensitivity: AlarmSensitivity.low,
          guardDog: newDog,
        );

        expect(newConditions.sensitivity.name, 'Low');
        expect(newConditions.guardDog.name, 'Rex');
      });
    });

    group('constants', () {
      test('verification window is 3 seconds', () {
        expect(
          AlarmConditions.verificationWindow,
          const Duration(seconds: 3),
        );
      });

      test('motions to confirm is 2', () {
        expect(AlarmConditions.motionsToConfirm, 2);
      });

      test('immediate threshold is 0.8', () {
        expect(AlarmConditions.immediateThreshold, 0.8);
      });
    });

    group('sensitivity levels', () {
      test('high sensitivity triggers on lower intensity', () {
        final highSensitivity = AlarmConditions(
          sensitivity: AlarmSensitivity.high,
          guardDog: testDog,
        );

        final event = MotionEvent(
          intensity: 0.4, // Above high threshold (0.3) but below medium (0.5)
          type: MotionType.shake,
          timestamp: DateTime.now(),
          triggerReading: SensorReading(
            x: 0.5,
            y: 0.5,
            z: 0.5,
            timestamp: DateTime.now(),
            type: SensorType.accelerometer,
          ),
        );

        final decision = highSensitivity.evaluateMotion(
          event,
          AlarmMode.standard,
        );

        expect(decision.shouldTrigger, false);
        expect(decision.reason, TriggerReason.needsVerification);
      });

      test('low sensitivity requires higher intensity', () {
        final lowSensitivity = AlarmConditions(
          sensitivity: AlarmSensitivity.low,
          guardDog: testDog,
        );

        final event = MotionEvent(
          intensity: 0.6, // Below low threshold (0.7)
          type: MotionType.shake,
          timestamp: DateTime.now(),
          triggerReading: SensorReading(
            x: 0.5,
            y: 0.5,
            z: 0.5,
            timestamp: DateTime.now(),
            type: SensorType.accelerometer,
          ),
        );

        final decision = lowSensitivity.evaluateMotion(
          event,
          AlarmMode.standard,
        );

        expect(decision.shouldTrigger, false);
        expect(decision.reason, TriggerReason.belowThreshold);
      });
    });
  });

  group('TriggerDecision', () {
    test('can be created with const constructor', () {
      const decision = TriggerDecision(
        shouldTrigger: true,
        reason: TriggerReason.highIntensity,
        effectiveIntensity: 0.9,
      );

      expect(decision.shouldTrigger, true);
      expect(decision.reason, TriggerReason.highIntensity);
      expect(decision.effectiveIntensity, 0.9);
    });

    test('trigger factory constructor sets shouldTrigger to true', () {
      const decision = TriggerDecision.trigger(
        TriggerReason.highIntensity,
        0.9,
      );

      expect(decision.shouldTrigger, true);
      expect(decision.reason, TriggerReason.highIntensity);
      expect(decision.effectiveIntensity, 0.9);
    });

    test('verify factory constructor sets correct reason', () {
      const decision = TriggerDecision.verify(0.6);

      expect(decision.shouldTrigger, false);
      expect(decision.reason, TriggerReason.needsVerification);
      expect(decision.effectiveIntensity, 0.6);
    });

    test('ignore factory constructor sets correct reason', () {
      const decision = TriggerDecision.ignore(0.3);

      expect(decision.shouldTrigger, false);
      expect(decision.reason, TriggerReason.belowThreshold);
      expect(decision.effectiveIntensity, 0.3);
    });
  });
}
