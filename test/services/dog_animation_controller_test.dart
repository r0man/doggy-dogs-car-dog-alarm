import 'package:flutter_test/flutter_test.dart';
import 'package:doggy_dogs_car_alarm/services/dog_animation_controller.dart';
import 'package:doggy_dogs_car_alarm/models/alarm_state.dart';
import 'package:doggy_dogs_car_alarm/models/dog.dart';
import 'package:doggy_dogs_car_alarm/models/dog_animation_state.dart';

void main() {
  group('DogAnimationController', () {
    late DogAnimationController controller;
    bool disposed = false;

    setUp(() {
      controller = DogAnimationController();
      disposed = false;
    });

    tearDown(() {
      if (!disposed) {
        controller.dispose();
      }
    });

    test('starts in idle state', () {
      expect(controller.currentState, DogAnimationState.idle);
      expect(controller.previousState, isNull);
      expect(controller.isTransitioning, isFalse);
    });

    group('updateFromAlarmState', () {
      test('transitions to barking when alarm triggered', () {
        const alarmState = AlarmState(
          isActive: true,
          isTriggered: true,
        );

        controller.updateFromAlarmState(alarmState);

        expect(controller.currentState, DogAnimationState.barking);
      });

      test('transitions to alert when countdown active', () {
        const alarmState = AlarmState(
          isCountingDown: true,
          countdownSeconds: 25,
        );

        controller.updateFromAlarmState(alarmState);

        expect(controller.currentState, DogAnimationState.alert);
      });

      test('transitions to alert when alarm active but not triggered', () {
        const alarmState = AlarmState(
          isActive: true,
          isTriggered: false,
        );

        controller.updateFromAlarmState(alarmState);

        expect(controller.currentState, DogAnimationState.alert);
      });

      test('transitions to idle when alarm inactive', () {
        // First activate alarm (to alert)
        controller.updateFromAlarmState(const AlarmState(isActive: true));
        expect(controller.currentState, DogAnimationState.alert);

        // Then deactivate
        controller.updateFromAlarmState(const AlarmState());
        expect(controller.currentState, DogAnimationState.idle);
      });

      test('triggered alarm overrides countdown', () {
        const alarmState = AlarmState(
          isActive: true,
          isCountingDown: true,
          isTriggered: true,
        );

        controller.updateFromAlarmState(alarmState);

        expect(controller.currentState, DogAnimationState.barking);
      });
    });

    group('updateFromMood', () {
      test('maps happy mood to happy animation', () {
        controller.updateFromMood(DogMood.happy);
        expect(controller.currentState, DogAnimationState.happy);
      });

      test('maps excited mood to happy animation', () {
        controller.updateFromMood(DogMood.excited);
        expect(controller.currentState, DogAnimationState.happy);
      });

      test('maps alert mood to alert animation', () {
        controller.updateFromMood(DogMood.alert);
        expect(controller.currentState, DogAnimationState.alert);
      });

      test('maps worried mood to sad animation', () {
        controller.updateFromMood(DogMood.worried);
        expect(controller.currentState, DogAnimationState.sad);
      });

      test('maps sad mood to sad animation', () {
        controller.updateFromMood(DogMood.sad);
        expect(controller.currentState, DogAnimationState.sad);
      });

      test('maps sleeping mood to sleeping animation', () {
        controller.updateFromMood(DogMood.sleeping);
        expect(controller.currentState, DogAnimationState.sleeping);
      });

      test('maps grumpy mood to sad animation', () {
        controller.updateFromMood(DogMood.grumpy);
        expect(controller.currentState, DogAnimationState.sad);
      });

      test('maps content mood to idle animation', () {
        controller.updateFromMood(DogMood.content);
        expect(controller.currentState, DogAnimationState.idle);
      });

      test('does not override higher priority alarm state', () {
        // Set to barking (high priority)
        controller.updateFromAlarmState(const AlarmState(
          isActive: true,
          isTriggered: true,
        ));
        expect(controller.currentState, DogAnimationState.barking);

        // Try to change mood
        controller.updateFromMood(DogMood.happy);

        // Should still be barking
        expect(controller.currentState, DogAnimationState.barking);
      });

      test('does not override alert state with lower priority mood', () {
        // Set to alert
        controller.updateFromAlarmState(const AlarmState(isActive: true));
        expect(controller.currentState, DogAnimationState.alert);

        // Try to change to happy (lower priority)
        controller.updateFromMood(DogMood.happy);

        // Should still be alert
        expect(controller.currentState, DogAnimationState.alert);
      });
    });

    group('playOnce', () {
      test('transitions to one-time animation', () async {
        await controller.playOnce(DogAnimationState.eating);
        expect(controller.currentState, DogAnimationState.eating);
      });

      test('one-time animations do not affect looping animations', () async {
        controller.playOnce(DogAnimationState.barking);
        expect(controller.currentState, DogAnimationState.barking);

        // Barking is looping, so should stay
        await Future.delayed(const Duration(seconds: 1));
        expect(controller.currentState, DogAnimationState.barking);
      });

      test('stores previous state before transition', () {
        controller.updateFromMood(DogMood.happy);
        expect(controller.currentState, DogAnimationState.happy);

        controller.forceState(DogAnimationState.sleeping);
        expect(controller.previousState, DogAnimationState.happy);
      });
    });

    group('forceState', () {
      test('forces transition regardless of priority', () {
        // Set to barking (highest priority)
        controller.updateFromAlarmState(const AlarmState(
          isActive: true,
          isTriggered: true,
        ));
        expect(controller.currentState, DogAnimationState.barking);

        // Force to sleeping (low priority)
        controller.forceState(DogAnimationState.sleeping);

        expect(controller.currentState, DogAnimationState.sleeping);
      });

      test('forces transition to same state', () {
        controller.forceState(DogAnimationState.idle);
        controller.forceState(DogAnimationState.idle);

        expect(controller.currentState, DogAnimationState.idle);
      });
    });

    group('transition behavior', () {
      test('ignores transition to same state', () {
        controller.updateFromMood(DogMood.happy);
        final previous = controller.previousState;

        controller.updateFromMood(DogMood.happy);

        // Should not update previousState if state didn't change
        expect(controller.currentState, DogAnimationState.happy);
      });

      test('respects priority system', () {
        // Set low priority state
        controller.updateFromMood(DogMood.sleeping);
        expect(controller.currentState, DogAnimationState.sleeping);

        // Try to transition to higher priority
        controller.updateFromAlarmState(const AlarmState(isActive: true));

        // Should succeed
        expect(controller.currentState, DogAnimationState.alert);
      });

      test('getTransitionDuration returns fast duration for barking', () {
        final duration = controller.getTransitionDuration(
          DogAnimationState.idle,
          DogAnimationState.barking,
        );

        expect(duration.inMilliseconds, lessThanOrEqualTo(200));
      });

      test('getTransitionDuration returns slow duration for sleeping', () {
        final duration = controller.getTransitionDuration(
          DogAnimationState.alert,
          DogAnimationState.sleeping,
        );

        expect(duration.inMilliseconds, greaterThanOrEqualTo(800));
      });

      test('getTransitionDuration returns default for other transitions', () {
        final duration = controller.getTransitionDuration(
          DogAnimationState.idle,
          DogAnimationState.happy,
        );

        expect(duration.inMilliseconds, equals(400));
      });
    });

    group('canTransitionTo', () {
      test('barking can always transition', () {
        controller.forceState(DogAnimationState.sleeping);

        expect(
          controller.canTransitionTo(DogAnimationState.barking),
          isTrue,
        );
      });

      test('cannot transition to lower priority state', () {
        controller.updateFromAlarmState(const AlarmState(isActive: true));
        expect(controller.currentState, DogAnimationState.alert);

        expect(
          controller.canTransitionTo(DogAnimationState.sleeping),
          isFalse,
        );
      });

      test('can transition to equal or higher priority state', () {
        controller.updateFromMood(DogMood.happy);

        expect(
          controller.canTransitionTo(DogAnimationState.alert),
          isTrue,
        );
      });
    });

    group('state tracking', () {
      test('tracks previous state correctly', () {
        controller.updateFromMood(DogMood.happy);
        controller.updateFromMood(DogMood.sad);

        expect(controller.previousState, DogAnimationState.happy);
        expect(controller.currentState, DogAnimationState.sad);
      });

      test('sets isTransitioning during transition', () async {
        var wasTransitioning = false;

        controller.addListener(() {
          if (controller.isTransitioning) {
            wasTransitioning = true;
          }
        });

        controller.updateFromMood(DogMood.happy);

        // Wait briefly for transition to complete
        await Future.delayed(const Duration(milliseconds: 50));

        expect(wasTransitioning, isTrue,
            reason: 'isTransitioning should be true during transition');
      });

      test('clears isTransitioning after transition', () async {
        controller.updateFromMood(DogMood.happy);

        // Wait for transition to complete (300ms + buffer)
        await Future.delayed(const Duration(milliseconds: 400));

        expect(controller.isTransitioning, isFalse);
      });
    });

    group('dispose', () {
      test('can be disposed safely', () {
        expect(() {
          controller.dispose();
          disposed = true;
        }, returnsNormally);
      });

      test('stops timers on dispose', () async {
        controller.playOnce(DogAnimationState.eating);
        controller.dispose();
        disposed = true;

        // Should not crash if timer tries to fire after dispose
        await Future.delayed(const Duration(seconds: 4));
      });
    });
  });
}
