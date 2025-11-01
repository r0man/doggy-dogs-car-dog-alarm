import 'package:flutter_test/flutter_test.dart';
import 'package:doggy_dogs_car_alarm/services/alarm_state_manager.dart';
import 'package:doggy_dogs_car_alarm/models/alarm_state.dart';

void main() {
  group('AlarmStateManager Tests', () {
    late AlarmStateManager manager;

    setUp(() {
      manager = AlarmStateManager();
    });

    tearDown(() {
      manager.dispose();
    });

    test('starts with default inactive state', () {
      expect(manager.currentState.isActive, false);
      expect(manager.currentState.isTriggered, false);
      expect(manager.currentState.isCountingDown, false);
    });

    test('emits state changes through stream', () async {
      final states = <AlarmState>[];
      final subscription = manager.stateStream.listen(states.add);

      manager.startCountdown(AlarmMode.standard, 30);
      await Future.delayed(const Duration(milliseconds: 10));

      expect(states.length, 1);
      expect(states[0].isCountingDown, true);

      await subscription.cancel();
    });

    group('canStartActivation', () {
      test('returns true when alarm is inactive', () {
        expect(manager.canStartActivation(), true);
      });

      test('returns false when countdown is active', () {
        manager.startCountdown(AlarmMode.standard, 30);
        expect(manager.canStartActivation(), false);
      });

      test('returns false when alarm is active', () {
        manager.startCountdown(AlarmMode.standard, 30);
        manager.completeActivation(AlarmMode.standard);
        expect(manager.canStartActivation(), false);
      });
    });

    group('startCountdown', () {
      test('starts countdown with correct mode and duration', () {
        final state = manager.startCountdown(AlarmMode.aggressive, 45);

        expect(state.isCountingDown, true);
        expect(state.countdownSeconds, 45);
        expect(state.mode, AlarmMode.aggressive);
        expect(manager.currentState, state);
      });

      test('throws StateError if countdown already started', () {
        manager.startCountdown(AlarmMode.standard, 30);

        expect(
          () => manager.startCountdown(AlarmMode.aggressive, 30),
          throwsStateError,
        );
      });

      test('throws StateError if alarm is active', () {
        manager.startCountdown(AlarmMode.standard, 30);
        manager.completeActivation(AlarmMode.standard);

        expect(
          () => manager.startCountdown(AlarmMode.aggressive, 30),
          throwsStateError,
        );
      });
    });

    group('updateCountdown', () {
      test('updates countdown seconds', () {
        manager.startCountdown(AlarmMode.standard, 30);
        final state = manager.updateCountdown(25);

        expect(state.countdownSeconds, 25);
        expect(state.isCountingDown, true);
      });

      test('throws StateError if countdown not active', () {
        expect(
          () => manager.updateCountdown(25),
          throwsStateError,
        );
      });
    });

    group('canCompleteCountdown', () {
      test('returns true when countdown is active', () {
        manager.startCountdown(AlarmMode.standard, 30);
        expect(manager.canCompleteCountdown(), true);
      });

      test('returns false when countdown is not active', () {
        expect(manager.canCompleteCountdown(), false);
      });
    });

    group('completeActivation', () {
      test('activates alarm after countdown', () {
        manager.startCountdown(AlarmMode.stealth, 30);
        final state = manager.completeActivation(AlarmMode.stealth);

        expect(state.isActive, true);
        expect(state.isCountingDown, false);
        expect(state.countdownSeconds, 0);
        expect(state.mode, AlarmMode.stealth);
        expect(state.activatedAt, isNotNull);
      });

      test('throws StateError if countdown not active', () {
        expect(
          () => manager.completeActivation(AlarmMode.standard),
          throwsStateError,
        );
      });
    });

    group('canCancelCountdown', () {
      test('returns true when countdown is active', () {
        manager.startCountdown(AlarmMode.standard, 30);
        expect(manager.canCancelCountdown(), true);
      });

      test('returns false when countdown is not active', () {
        expect(manager.canCancelCountdown(), false);
      });
    });

    group('cancelCountdown', () {
      test('cancels active countdown', () {
        manager.startCountdown(AlarmMode.standard, 30);
        final state = manager.cancelCountdown();

        expect(state.isCountingDown, false);
        expect(state.countdownSeconds, 0);
      });

      test('throws StateError if countdown not active', () {
        expect(
          () => manager.cancelCountdown(),
          throwsStateError,
        );
      });
    });

    group('canDeactivate', () {
      test('returns true when alarm is active', () {
        manager.startCountdown(AlarmMode.standard, 30);
        manager.completeActivation(AlarmMode.standard);
        expect(manager.canDeactivate(), true);
      });

      test('returns false when alarm is not active', () {
        expect(manager.canDeactivate(), false);
      });
    });

    group('deactivate', () {
      test('deactivates active alarm', () {
        manager.startCountdown(AlarmMode.standard, 30);
        manager.completeActivation(AlarmMode.standard);
        final state = manager.deactivate();

        expect(state.isActive, false);
        expect(state.isTriggered, false);
        expect(state.activatedAt, null);
      });

      test('throws StateError if alarm not active', () {
        expect(
          () => manager.deactivate(),
          throwsStateError,
        );
      });
    });

    group('canTrigger', () {
      test('returns true when alarm is active and not triggered', () {
        manager.startCountdown(AlarmMode.standard, 30);
        manager.completeActivation(AlarmMode.standard);
        expect(manager.canTrigger(), true);
      });

      test('returns false when alarm is not active', () {
        expect(manager.canTrigger(), false);
      });

      test('returns false when alarm is already triggered', () {
        manager.startCountdown(AlarmMode.standard, 30);
        manager.completeActivation(AlarmMode.standard);
        manager.trigger();
        expect(manager.canTrigger(), false);
      });
    });

    group('trigger', () {
      test('triggers active alarm', () {
        manager.startCountdown(AlarmMode.standard, 30);
        manager.completeActivation(AlarmMode.standard);
        final state = manager.trigger();

        expect(state.isTriggered, true);
        expect(state.triggerCount, 1);
        expect(state.lastTriggeredAt, isNotNull);
      });

      test('returns current state if already triggered (no-op)', () {
        manager.startCountdown(AlarmMode.standard, 30);
        manager.completeActivation(AlarmMode.standard);
        manager.trigger();
        final state2 = manager.trigger();

        expect(state2.triggerCount, 1); // Not incremented again
      });

      test('increments trigger count on multiple triggers', () {
        manager.startCountdown(AlarmMode.standard, 30);
        manager.completeActivation(AlarmMode.standard);
        manager.trigger();
        manager.acknowledge();
        final state = manager.trigger();

        expect(state.triggerCount, 2);
      });
    });

    group('canAcknowledge', () {
      test('returns true when alarm is triggered', () {
        manager.startCountdown(AlarmMode.standard, 30);
        manager.completeActivation(AlarmMode.standard);
        manager.trigger();
        expect(manager.canAcknowledge(), true);
      });

      test('returns false when alarm is not triggered', () {
        expect(manager.canAcknowledge(), false);
      });
    });

    group('acknowledge', () {
      test('acknowledges triggered alarm', () {
        manager.startCountdown(AlarmMode.standard, 30);
        manager.completeActivation(AlarmMode.standard);
        manager.trigger();
        final state = manager.acknowledge();

        expect(state.isTriggered, false);
        expect(state.isActive, true); // Still active
      });

      test('throws StateError if alarm not triggered', () {
        expect(
          () => manager.acknowledge(),
          throwsStateError,
        );
      });
    });

    group('setState', () {
      test('sets state directly', () {
        const customState = AlarmState(
          isActive: true,
          mode: AlarmMode.aggressive,
          triggerCount: 5,
        );

        manager.setState(customState);

        expect(manager.currentState.isActive, true);
        expect(manager.currentState.mode, AlarmMode.aggressive);
        expect(manager.currentState.triggerCount, 5);
      });

      test('emits state through stream', () async {
        final states = <AlarmState>[];
        final subscription = manager.stateStream.listen(states.add);

        const customState = AlarmState(isActive: true);
        manager.setState(customState);
        await Future.delayed(const Duration(milliseconds: 10));

        expect(states.length, 1);
        expect(states[0].isActive, true);

        await subscription.cancel();
      });
    });

    group('full lifecycle', () {
      test('completes full alarm activation and deactivation cycle', () async {
        final states = <AlarmState>[];
        final subscription = manager.stateStream.listen(states.add);

        // Start countdown
        manager.startCountdown(AlarmMode.standard, 30);
        await Future.delayed(const Duration(milliseconds: 10));

        // Complete activation
        manager.completeActivation(AlarmMode.standard);
        await Future.delayed(const Duration(milliseconds: 10));

        // Trigger alarm
        manager.trigger();
        await Future.delayed(const Duration(milliseconds: 10));

        // Acknowledge
        manager.acknowledge();
        await Future.delayed(const Duration(milliseconds: 10));

        // Deactivate
        manager.deactivate();
        await Future.delayed(const Duration(milliseconds: 10));

        expect(states.length, 5);
        expect(states[0].isCountingDown, true);
        expect(states[1].isActive, true);
        expect(states[2].isTriggered, true);
        expect(states[3].isTriggered, false);
        expect(states[4].isActive, false);

        await subscription.cancel();
      });
    });
  });
}
