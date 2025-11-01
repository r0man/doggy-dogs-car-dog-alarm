import 'package:flutter_test/flutter_test.dart';
import 'package:doggy_dogs_car_alarm/models/dog_animation_state.dart';

void main() {
  group('DogAnimationState Tests', () {
    test('all animation states are defined', () {
      expect(DogAnimationState.values.length, 8);
      expect(DogAnimationState.values.contains(DogAnimationState.idle), true);
      expect(DogAnimationState.values.contains(DogAnimationState.alert), true);
      expect(
          DogAnimationState.values.contains(DogAnimationState.barking), true);
      expect(DogAnimationState.values.contains(DogAnimationState.happy), true);
      expect(DogAnimationState.values.contains(DogAnimationState.sad), true);
      expect(
          DogAnimationState.values.contains(DogAnimationState.sleeping), true);
      expect(DogAnimationState.values.contains(DogAnimationState.eating), true);
      expect(
          DogAnimationState.values.contains(DogAnimationState.playing), true);
    });
  });

  group('DogAnimationStateExtension Tests', () {
    test('display names are correct', () {
      expect(DogAnimationState.idle.displayName, 'Idle');
      expect(DogAnimationState.alert.displayName, 'Alert');
      expect(DogAnimationState.barking.displayName, 'Barking');
      expect(DogAnimationState.happy.displayName, 'Happy');
      expect(DogAnimationState.sad.displayName, 'Sad');
      expect(DogAnimationState.sleeping.displayName, 'Sleeping');
      expect(DogAnimationState.eating.displayName, 'Eating');
      expect(DogAnimationState.playing.displayName, 'Playing');
    });

    test('descriptions are provided for all states', () {
      for (final state in DogAnimationState.values) {
        expect(state.description, isNotEmpty);
        expect(state.description.length, greaterThan(10));
      }
    });

    test('looping states are correct', () {
      // These should loop continuously
      expect(DogAnimationState.idle.isLooping, true);
      expect(DogAnimationState.alert.isLooping, true);
      expect(DogAnimationState.barking.isLooping, true);
      expect(DogAnimationState.happy.isLooping, true);
      expect(DogAnimationState.sad.isLooping, true);
      expect(DogAnimationState.sleeping.isLooping, true);

      // These play once and return to idle
      expect(DogAnimationState.eating.isLooping, false);
      expect(DogAnimationState.playing.isLooping, false);
    });

    test('non-looping animations have duration', () {
      expect(DogAnimationState.eating.duration, 3.0);
      expect(DogAnimationState.playing.duration, 2.5);
    });

    test('looping animations have zero duration', () {
      expect(DogAnimationState.idle.duration, 0.0);
      expect(DogAnimationState.alert.duration, 0.0);
      expect(DogAnimationState.barking.duration, 0.0);
      expect(DogAnimationState.happy.duration, 0.0);
      expect(DogAnimationState.sad.duration, 0.0);
      expect(DogAnimationState.sleeping.duration, 0.0);
    });

    test('priority levels are assigned correctly', () {
      // Barking has highest priority (alarm)
      expect(DogAnimationState.barking.priority, 100);

      // Alert is second highest
      expect(DogAnimationState.alert.priority, 90);

      // Eating has medium priority
      expect(DogAnimationState.eating.priority, 60);

      // Playing has medium priority
      expect(DogAnimationState.playing.priority, 50);

      // Happy and sad have same lower priority
      expect(DogAnimationState.happy.priority, 40);
      expect(DogAnimationState.sad.priority, 40);

      // Idle has low priority
      expect(DogAnimationState.idle.priority, 20);

      // Sleeping has lowest priority (easily interrupted)
      expect(DogAnimationState.sleeping.priority, 10);
    });

    test('barking has highest priority', () {
      for (final state in DogAnimationState.values) {
        if (state != DogAnimationState.barking) {
          expect(
            DogAnimationState.barking.priority > state.priority,
            true,
            reason: 'barking should have higher priority than $state',
          );
        }
      }
    });

    test('sleeping has lowest priority', () {
      for (final state in DogAnimationState.values) {
        if (state != DogAnimationState.sleeping) {
          expect(
            DogAnimationState.sleeping.priority < state.priority,
            true,
            reason: 'sleeping should have lower priority than $state',
          );
        }
      }
    });

    test('priority ordering makes sense for interruptions', () {
      // Alarm states should interrupt everything
      expect(
          DogAnimationState.barking.priority >
              DogAnimationState.eating.priority,
          true);
      expect(
          DogAnimationState.alert.priority > DogAnimationState.playing.priority,
          true);

      // Activities should interrupt idle
      expect(
          DogAnimationState.eating.priority > DogAnimationState.idle.priority,
          true);
      expect(
          DogAnimationState.playing.priority > DogAnimationState.idle.priority,
          true);

      // Everything should interrupt sleeping
      expect(
          DogAnimationState.idle.priority > DogAnimationState.sleeping.priority,
          true);
      expect(
          DogAnimationState.happy.priority >
              DogAnimationState.sleeping.priority,
          true);
    });
  });
}
