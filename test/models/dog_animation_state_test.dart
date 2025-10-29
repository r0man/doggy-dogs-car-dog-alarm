import 'package:flutter_test/flutter_test.dart';
import 'package:doggy_dogs_car_alarm/models/dog_animation_state.dart';

void main() {
  group('DogAnimationState', () {
    test('has correct display names', () {
      expect(DogAnimationState.idle.displayName, 'Idle');
      expect(DogAnimationState.alert.displayName, 'Alert');
      expect(DogAnimationState.barking.displayName, 'Barking');
      expect(DogAnimationState.happy.displayName, 'Happy');
      expect(DogAnimationState.sad.displayName, 'Sad');
      expect(DogAnimationState.sleeping.displayName, 'Sleeping');
      expect(DogAnimationState.eating.displayName, 'Eating');
      expect(DogAnimationState.playing.displayName, 'Playing');
    });

    test('has descriptions for all states', () {
      for (final state in DogAnimationState.values) {
        expect(state.description.isNotEmpty, isTrue,
            reason: '${state.displayName} should have a description');
      }
    });

    test('looping states loop continuously', () {
      expect(DogAnimationState.idle.isLooping, isTrue);
      expect(DogAnimationState.alert.isLooping, isTrue);
      expect(DogAnimationState.barking.isLooping, isTrue);
      expect(DogAnimationState.happy.isLooping, isTrue);
      expect(DogAnimationState.sad.isLooping, isTrue);
      expect(DogAnimationState.sleeping.isLooping, isTrue);
    });

    test('one-time animations do not loop', () {
      expect(DogAnimationState.eating.isLooping, isFalse);
      expect(DogAnimationState.playing.isLooping, isFalse);
    });

    test('one-time animations have duration', () {
      expect(DogAnimationState.eating.duration, greaterThan(0));
      expect(DogAnimationState.playing.duration, greaterThan(0));
    });

    test('looping animations have zero duration', () {
      expect(DogAnimationState.idle.duration, 0.0);
      expect(DogAnimationState.alert.duration, 0.0);
      expect(DogAnimationState.sleeping.duration, 0.0);
    });

    test('barking has highest priority', () {
      for (final state in DogAnimationState.values) {
        if (state != DogAnimationState.barking) {
          expect(
            DogAnimationState.barking.priority,
            greaterThan(state.priority),
            reason: 'Barking should have higher priority than ${state.displayName}',
          );
        }
      }
    });

    test('alert has second highest priority', () {
      final nonAlertStates = DogAnimationState.values
          .where((s) => s != DogAnimationState.barking && s != DogAnimationState.alert)
          .toList();

      for (final state in nonAlertStates) {
        expect(
          DogAnimationState.alert.priority,
          greaterThan(state.priority),
          reason: 'Alert should have higher priority than ${state.displayName}',
        );
      }
    });

    test('sleeping has low priority', () {
      expect(DogAnimationState.sleeping.priority, lessThan(50));
    });

    test('idle has low priority', () {
      expect(DogAnimationState.idle.priority, lessThan(50));
    });

    test('priority system allows interruptions', () {
      // Higher priority should be able to interrupt lower priority
      expect(
        DogAnimationState.barking.priority > DogAnimationState.sleeping.priority,
        isTrue,
        reason: 'Barking should interrupt sleeping',
      );

      expect(
        DogAnimationState.alert.priority > DogAnimationState.happy.priority,
        isTrue,
        reason: 'Alert should interrupt happy',
      );

      expect(
        DogAnimationState.eating.priority > DogAnimationState.idle.priority,
        isTrue,
        reason: 'Eating should interrupt idle',
      );
    });

    test('all states have unique display names', () {
      final displayNames = DogAnimationState.values.map((s) => s.displayName).toSet();
      expect(displayNames.length, equals(DogAnimationState.values.length),
          reason: 'Each state should have a unique display name');
    });

    test('all states have positive or zero priority', () {
      for (final state in DogAnimationState.values) {
        expect(state.priority, greaterThanOrEqualTo(0),
            reason: '${state.displayName} should have non-negative priority');
      }
    });
  });
}
