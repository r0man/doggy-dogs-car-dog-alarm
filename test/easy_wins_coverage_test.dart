/// Targeted tests to increase coverage for files with small gaps
library;

import 'package:flutter_test/flutter_test.dart';
import 'package:doggy_dogs_car_alarm/models/alarm_state.dart';
import 'package:doggy_dogs_car_alarm/models/sensor_data.dart';
import 'package:doggy_dogs_car_alarm/models/dog.dart';
import 'package:doggy_dogs_car_alarm/services/alarm_conditions.dart';

void main() {
  group('AlarmState - Complete Coverage', () {
    test('toString includes all relevant state', () {
      const state = AlarmState(
        isActive: true,
        isTriggered: false,
        isCountingDown: true,
        countdownSeconds: 30,
        triggerCount: 5,
        mode: AlarmMode.aggressive,
      );

      final str = state.toString();
      expect(str, contains('isActive: true'));
      expect(str, contains('isTriggered: false'));
      expect(str, contains('isCountingDown: true'));
      expect(str, contains('countdownSeconds: 30'));
      expect(str, contains('triggerCount: 5'));
      expect(str, contains('AlarmMode.aggressive'));
    });

    test('activeDuration returns null when not activated', () {
      const state = AlarmState();
      expect(state.activeDuration, isNull);
    });

    test('activeDuration returns duration when activated', () {
      final activatedTime = DateTime.now().subtract(const Duration(minutes: 5));
      final state = AlarmState(
        isActive: true,
        activatedAt: activatedTime,
      );

      final duration = state.activeDuration!;
      expect(duration.inMinutes, greaterThanOrEqualTo(4));
      expect(duration.inMinutes, lessThanOrEqualTo(6));
    });

    test('timeSinceLastTrigger returns null when never triggered', () {
      const state = AlarmState();
      expect(state.timeSinceLastTrigger, isNull);
    });

    test('timeSinceLastTrigger returns duration when triggered', () {
      final triggerTime = DateTime.now().subtract(const Duration(seconds: 30));
      final state = AlarmState(
        isTriggered: true,
        lastTriggeredAt: triggerTime,
      );

      final duration = state.timeSinceLastTrigger!;
      expect(duration.inSeconds, greaterThanOrEqualTo(29));
      expect(duration.inSeconds, lessThanOrEqualTo(31));
    });
  });

  group('AlarmMode - Complete Coverage', () {
    test('all modes have display names', () {
      for (final mode in AlarmMode.values) {
        expect(mode.displayName, isNotEmpty);
      }
    });

    test('all modes have descriptions', () {
      for (final mode in AlarmMode.values) {
        expect(mode.description, isNotEmpty);
      }
    });

    test('barkIntensity values are correct', () {
      expect(AlarmMode.standard.barkIntensity, 1.0);
      expect(AlarmMode.stealth.barkIntensity, 0.0);
      expect(AlarmMode.aggressive.barkIntensity, 1.5);
    });

    test('hasDelayedResponse values are correct', () {
      expect(AlarmMode.standard.hasDelayedResponse, true);
      expect(AlarmMode.stealth.hasDelayedResponse, true);
      expect(AlarmMode.aggressive.hasDelayedResponse, false);
    });
  });

  group('SensorData - Complete Coverage', () {
    test('SensorReading toString includes type and values', () {
      final reading = SensorReading(
        x: 1.23,
        y: 4.56,
        z: 7.89,
        timestamp: DateTime.now(),
        type: SensorType.accelerometer,
      );

      final str = reading.toString();
      expect(str, contains('SensorReading'));
      expect(str, contains('accelerometer'));
      expect(str, contains('x=1.23'));
      expect(str, contains('y=4.56'));
      expect(str, contains('z=7.89'));
      expect(str, contains('magnitude'));
    });

    test('MotionEvent toString includes type and intensity', () {
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

    test('AlarmSensitivity copyWith updates values correctly', () {
      const original = AlarmSensitivity.medium;

      final modified = original.copyWith(
        name: 'Custom',
        threshold: 0.6,
      );

      expect(modified.name, 'Custom');
      expect(modified.threshold, 0.6);
      expect(modified.accelerometerThreshold, original.accelerometerThreshold);
    });
  });

  group('AlarmConditions - Complete Coverage', () {
    test('evaluateMotion returns ignore for motion below threshold', () {
      final dog = Dog(
        id: 'test',
        name: 'Guard',
        breed: DogBreed.germanShepherd,
        stats:
            const DogStats(hunger: 80, happiness: 80, energy: 80, loyalty: 80),
        personality: const DogPersonality(protective: true),
        createdAt: DateTime.now(),
        lastInteraction: DateTime.now(),
      );

      final conditions = AlarmConditions(
        sensitivity: AlarmSensitivity.medium,
        guardDog: dog,
      );

      final lowIntensityEvent = MotionEvent(
        intensity: 0.1, // Below medium threshold of 0.5
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

      final decision =
          conditions.evaluateMotion(lowIntensityEvent, AlarmMode.standard);
      expect(decision.shouldTrigger, false);
      expect(decision.reason, TriggerReason.belowThreshold);
    });

    test('evaluateMotion triggers immediately in aggressive mode', () {
      final dog = Dog(
        id: 'test',
        name: 'Guard',
        breed: DogBreed.doberman,
        stats:
            const DogStats(hunger: 80, happiness: 80, energy: 80, loyalty: 80),
        personality: const DogPersonality(protective: true, brave: true),
        createdAt: DateTime.now(),
        lastInteraction: DateTime.now(),
      );

      final conditions = AlarmConditions(
        sensitivity: AlarmSensitivity.medium,
        guardDog: dog,
      );

      final event = MotionEvent(
        intensity: 0.6,
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

      final decision = conditions.evaluateMotion(event, AlarmMode.aggressive);
      expect(decision.shouldTrigger, true);
      expect(decision.reason, TriggerReason.aggressiveMode);
    });

    test('evaluateMotion triggers immediately for high intensity', () {
      final dog = Dog(
        id: 'test',
        name: 'Guard',
        breed: DogBreed.rottweiler,
        stats:
            const DogStats(hunger: 90, happiness: 90, energy: 90, loyalty: 90),
        personality: const DogPersonality(protective: true),
        createdAt: DateTime.now(),
        lastInteraction: DateTime.now(),
      );

      final conditions = AlarmConditions(
        sensitivity: AlarmSensitivity.low,
        guardDog: dog,
      );

      final highIntensityEvent = MotionEvent(
        intensity: 0.95, // Above immediate threshold of 0.8
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

      final decision =
          conditions.evaluateMotion(highIntensityEvent, AlarmMode.standard);
      expect(decision.shouldTrigger, true);
      expect(decision.reason, TriggerReason.highIntensity);
    });

    test('evaluateMotion returns verify for standard mode medium intensity',
        () {
      final dog = Dog(
        id: 'test',
        name: 'Guard',
        breed: DogBreed.germanShepherd,
        stats:
            const DogStats(hunger: 80, happiness: 80, energy: 80, loyalty: 80),
        personality: const DogPersonality(protective: true),
        createdAt: DateTime.now(),
        lastInteraction: DateTime.now(),
      );

      final conditions = AlarmConditions(
        sensitivity: AlarmSensitivity.medium,
        guardDog: dog,
      );

      final event = MotionEvent(
        intensity: 0.6,
        type: MotionType.tilt,
        timestamp: DateTime.now(),
        triggerReading: SensorReading(
          x: 3.0,
          y: 3.0,
          z: 3.0,
          timestamp: DateTime.now(),
          type: SensorType.accelerometer,
        ),
      );

      final decision = conditions.evaluateMotion(event, AlarmMode.standard);
      expect(decision.shouldTrigger, false);
      expect(decision.reason, TriggerReason.needsVerification);
    });

    test('isVerifiedThreat returns false with insufficient motions', () {
      final dog = Dog(
        id: 'test',
        name: 'Guard',
        breed: DogBreed.germanShepherd,
        stats:
            const DogStats(hunger: 80, happiness: 80, energy: 80, loyalty: 80),
        personality: const DogPersonality(protective: true),
        createdAt: DateTime.now(),
        lastInteraction: DateTime.now(),
      );

      final conditions = AlarmConditions(
        sensitivity: AlarmSensitivity.medium,
        guardDog: dog,
      );

      final singleMotion = [
        MotionEvent(
          intensity: 0.6,
          type: MotionType.shake,
          timestamp: DateTime.now(),
          triggerReading: SensorReading(
            x: 3.0,
            y: 3.0,
            z: 3.0,
            timestamp: DateTime.now(),
            type: SensorType.accelerometer,
          ),
        ),
      ];

      expect(conditions.isVerifiedThreat(singleMotion), false);
    });

    test('isVerifiedThreat returns true with sufficient recent motions', () {
      final dog = Dog(
        id: 'test',
        name: 'Guard',
        breed: DogBreed.germanShepherd,
        stats:
            const DogStats(hunger: 80, happiness: 80, energy: 80, loyalty: 80),
        personality: const DogPersonality(protective: true),
        createdAt: DateTime.now(),
        lastInteraction: DateTime.now(),
      );

      final conditions = AlarmConditions(
        sensitivity: AlarmSensitivity.medium,
        guardDog: dog,
      );

      final now = DateTime.now();
      final recentMotions = [
        MotionEvent(
          intensity: 0.6,
          type: MotionType.shake,
          timestamp: now.subtract(const Duration(seconds: 1)),
          triggerReading: SensorReading(
            x: 3.0,
            y: 3.0,
            z: 3.0,
            timestamp: now.subtract(const Duration(seconds: 1)),
            type: SensorType.accelerometer,
          ),
        ),
        MotionEvent(
          intensity: 0.7,
          type: MotionType.shake,
          timestamp: now,
          triggerReading: SensorReading(
            x: 4.0,
            y: 4.0,
            z: 4.0,
            timestamp: now,
            type: SensorType.accelerometer,
          ),
        ),
      ];

      expect(conditions.isVerifiedThreat(recentMotions), true);
    });

    test('filterRecentMotions removes old motions', () {
      final dog = Dog(
        id: 'test',
        name: 'Guard',
        breed: DogBreed.germanShepherd,
        stats:
            const DogStats(hunger: 80, happiness: 80, energy: 80, loyalty: 80),
        personality: const DogPersonality(protective: true),
        createdAt: DateTime.now(),
        lastInteraction: DateTime.now(),
      );

      final conditions = AlarmConditions(
        sensitivity: AlarmSensitivity.medium,
        guardDog: dog,
      );

      final now = DateTime.now();
      final motions = [
        MotionEvent(
          intensity: 0.6,
          type: MotionType.shake,
          timestamp: now.subtract(const Duration(seconds: 10)), // Too old
          triggerReading: SensorReading(
            x: 3.0,
            y: 3.0,
            z: 3.0,
            timestamp: now.subtract(const Duration(seconds: 10)),
            type: SensorType.accelerometer,
          ),
        ),
        MotionEvent(
          intensity: 0.7,
          type: MotionType.shake,
          timestamp: now, // Recent
          triggerReading: SensorReading(
            x: 4.0,
            y: 4.0,
            z: 4.0,
            timestamp: now,
            type: SensorType.accelerometer,
          ),
        ),
      ];

      final filtered = conditions.filterRecentMotions(motions);
      expect(filtered.length, 1);
      expect(filtered.first.intensity, 0.7);
    });

    test('copyWith creates new instance with updated values', () {
      final dog1 = Dog(
        id: 'test1',
        name: 'Guard1',
        breed: DogBreed.germanShepherd,
        stats:
            const DogStats(hunger: 80, happiness: 80, energy: 80, loyalty: 80),
        personality: const DogPersonality(protective: true),
        createdAt: DateTime.now(),
        lastInteraction: DateTime.now(),
      );

      final dog2 = Dog(
        id: 'test2',
        name: 'Guard2',
        breed: DogBreed.doberman,
        stats:
            const DogStats(hunger: 90, happiness: 90, energy: 90, loyalty: 90),
        personality: const DogPersonality(protective: true, brave: true),
        createdAt: DateTime.now(),
        lastInteraction: DateTime.now(),
      );

      final original = AlarmConditions(
        sensitivity: AlarmSensitivity.medium,
        guardDog: dog1,
      );

      final modified = original.copyWith(
        sensitivity: AlarmSensitivity.high,
        guardDog: dog2,
      );

      expect(modified.sensitivity, AlarmSensitivity.high);
      expect(modified.guardDog.id, 'test2');
    });
  });

  group('TriggerReason - Enum Coverage', () {
    test('all trigger reasons are defined', () {
      expect(TriggerReason.values.length, greaterThanOrEqualTo(5));
      expect(TriggerReason.values, contains(TriggerReason.belowThreshold));
      expect(TriggerReason.values, contains(TriggerReason.needsVerification));
      expect(TriggerReason.values, contains(TriggerReason.aggressiveMode));
      expect(TriggerReason.values, contains(TriggerReason.highIntensity));
      expect(TriggerReason.values, contains(TriggerReason.verifiedThreat));
    });
  });

  group('Uncovered Lines - Targeted Coverage', () {
    test('TriggerDecision regular constructor (alarm_conditions.dart:11)', () {
      // Test the regular constructor directly
      const decision = TriggerDecision(
        shouldTrigger: true,
        reason: TriggerReason.highIntensity,
        effectiveIntensity: 0.9,
      );

      expect(decision.shouldTrigger, true);
      expect(decision.reason, TriggerReason.highIntensity);
      expect(decision.effectiveIntensity, 0.9);
    });

    test('DogBreed.beagle displayName (dog.dart:135)', () {
      const breed = DogBreed.beagle;
      expect(breed.displayName, 'Beagle');
    });

    test('DogMood.grumpy message (dog.dart:332)', () {
      const mood = DogMood.grumpy;
      expect(mood.description, 'Your dog is grumpy');
    });

    test(
        'AlarmSensitivity copyWith with null parameters (sensor_data.dart:138-139)',
        () {
      const sensitivity = AlarmSensitivity(
        name: 'Test',
        threshold: 0.5,
        accelerometerThreshold: 10.0,
        gyroscopeThreshold: 5.0,
      );

      // Call copyWith without parameters to test the ?? operators
      final copied = sensitivity.copyWith();

      expect(copied.name, 'Test');
      expect(copied.threshold, 0.5);
      expect(copied.accelerometerThreshold, 10.0);
      expect(copied.gyroscopeThreshold, 5.0);
    });
  });
}
