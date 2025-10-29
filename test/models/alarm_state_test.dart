import 'package:flutter_test/flutter_test.dart';
import 'package:doggy_dogs_car_alarm/models/alarm_state.dart';

void main() {
  group('AlarmState', () {
    test('creates default inactive state', () {
      const state = AlarmState();

      expect(state.isActive, isFalse);
      expect(state.isTriggered, isFalse);
      expect(state.isCountingDown, isFalse);
      expect(state.countdownSeconds, 0);
      expect(state.activatedAt, isNull);
      expect(state.lastTriggeredAt, isNull);
      expect(state.triggerCount, 0);
      expect(state.mode, AlarmMode.standard);
    });

    test('starts countdown correctly', () {
      const state = AlarmState();
      final countdownState = state.startCountdown(AlarmMode.aggressive, 30);

      expect(countdownState.isCountingDown, isTrue);
      expect(countdownState.countdownSeconds, 30);
      expect(countdownState.mode, AlarmMode.aggressive);
      expect(countdownState.isActive, isFalse);
    });

    test('updates countdown seconds', () {
      const state = AlarmState();
      final countdownState = state.startCountdown(AlarmMode.standard, 30);
      final updatedState = countdownState.updateCountdown(25);

      expect(updatedState.isCountingDown, isTrue);
      expect(updatedState.countdownSeconds, 25);
    });

    test('cancels countdown', () {
      const state = AlarmState();
      final countdownState = state.startCountdown(AlarmMode.standard, 30);
      final cancelledState = countdownState.cancelCountdown();

      expect(cancelledState.isCountingDown, isFalse);
      expect(cancelledState.countdownSeconds, 0);
    });

    test('activates alarm after countdown', () {
      const state = AlarmState();
      final countdownState = state.startCountdown(AlarmMode.stealth, 30);
      final activeState = countdownState.activate(AlarmMode.stealth);

      expect(activeState.isActive, isTrue);
      expect(activeState.isCountingDown, isFalse);
      expect(activeState.countdownSeconds, 0);
      expect(activeState.mode, AlarmMode.stealth);
      expect(activeState.activatedAt, isNotNull);
    });

    test('deactivates alarm', () {
      const state = AlarmState();
      final activeState = state.activate(AlarmMode.standard);
      final deactivatedState = activeState.deactivate();

      expect(deactivatedState.isActive, isFalse);
      expect(deactivatedState.isTriggered, isFalse);
      expect(deactivatedState.isCountingDown, isFalse);
      expect(deactivatedState.activatedAt, isNull);
    });

    test('triggers alarm', () {
      const state = AlarmState();
      final activeState = state.activate(AlarmMode.standard);
      final triggeredState = activeState.trigger();

      expect(triggeredState.isTriggered, isTrue);
      expect(triggeredState.triggerCount, 1);
      expect(triggeredState.lastTriggeredAt, isNotNull);
    });

    test('acknowledges triggered alarm', () {
      const state = AlarmState();
      final activeState = state.activate(AlarmMode.standard);
      final triggeredState = activeState.trigger();
      final acknowledgedState = triggeredState.acknowledge();

      expect(acknowledgedState.isTriggered, isFalse);
      expect(acknowledgedState.isActive, isTrue);
      expect(acknowledgedState.triggerCount, 1);
    });

    test('increments trigger count on multiple triggers', () {
      const state = AlarmState();
      final activeState = state.activate(AlarmMode.standard);
      final triggeredOnce = activeState.trigger();
      final acknowledgedOnce = triggeredOnce.acknowledge();
      final triggeredTwice = acknowledgedOnce.trigger();

      expect(triggeredTwice.triggerCount, 2);
    });

    test('copyWith creates new instance with updated fields', () {
      const state = AlarmState(
        isActive: true,
        mode: AlarmMode.standard,
      );

      final newState = state.copyWith(mode: AlarmMode.aggressive);

      expect(newState.isActive, isTrue);
      expect(newState.mode, AlarmMode.aggressive);
    });

    test('calculates active duration correctly', () async {
      const state = AlarmState();
      final activeState = state.activate(AlarmMode.standard);

      await Future.delayed(const Duration(milliseconds: 100));

      expect(activeState.activeDuration, isNotNull);
      expect(activeState.activeDuration!.inMilliseconds,
          greaterThanOrEqualTo(100));
    });

    test('toString returns formatted string', () {
      const state = AlarmState(
        isActive: true,
        isTriggered: true,
        isCountingDown: false,
        countdownSeconds: 0,
        triggerCount: 2,
        mode: AlarmMode.aggressive,
      );

      final str = state.toString();

      expect(str, contains('isActive: true'));
      expect(str, contains('isTriggered: true'));
      expect(str, contains('isCountingDown: false'));
      expect(str, contains('countdownSeconds: 0'));
      expect(str, contains('triggerCount: 2'));
      expect(str, contains('mode: AlarmMode.aggressive'));
    });
  });

  group('AlarmMode', () {
    test('has correct display names', () {
      expect(AlarmMode.standard.displayName, 'Standard');
      expect(AlarmMode.stealth.displayName, 'Stealth');
      expect(AlarmMode.aggressive.displayName, 'Aggressive');
    });

    test('has correct descriptions', () {
      expect(AlarmMode.standard.description, contains('Normal alarm response'));
      expect(AlarmMode.stealth.description, contains('Silent notifications'));
      expect(
          AlarmMode.aggressive.description, contains('Immediate loud barking'));
    });

    test('has correct bark intensity', () {
      expect(AlarmMode.standard.barkIntensity, 1.0);
      expect(AlarmMode.stealth.barkIntensity, 0.0);
      expect(AlarmMode.aggressive.barkIntensity, 1.5);
    });

    test('has correct delayed response flag', () {
      expect(AlarmMode.standard.hasDelayedResponse, isTrue);
      expect(AlarmMode.stealth.hasDelayedResponse, isTrue);
      expect(AlarmMode.aggressive.hasDelayedResponse, isFalse);
    });
  });
}
