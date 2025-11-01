import 'package:flutter_test/flutter_test.dart';
import 'package:doggy_dogs_car_alarm/models/alarm_state.dart';

void main() {
  group('AlarmState Tests', () {
    test('creates default alarm state', () {
      const state = AlarmState();

      expect(state.isActive, false);
      expect(state.isTriggered, false);
      expect(state.isCountingDown, false);
      expect(state.countdownSeconds, 0);
      expect(state.activatedAt, null);
      expect(state.lastTriggeredAt, null);
      expect(state.triggerCount, 0);
      expect(state.mode, AlarmMode.standard);
    });

    test('copyWith creates new instance with updated values', () {
      const state = AlarmState();
      final newState = state.copyWith(
        isActive: true,
        mode: AlarmMode.aggressive,
        triggerCount: 5,
      );

      expect(newState.isActive, true);
      expect(newState.mode, AlarmMode.aggressive);
      expect(newState.triggerCount, 5);
      expect(newState.isTriggered, false); // unchanged
    });

    test('startCountdown sets countdown state', () {
      const state = AlarmState();
      final newState = state.startCountdown(AlarmMode.stealth, 30);

      expect(newState.isCountingDown, true);
      expect(newState.countdownSeconds, 30);
      expect(newState.mode, AlarmMode.stealth);
    });

    test('updateCountdown updates seconds', () {
      const state = AlarmState(isCountingDown: true, countdownSeconds: 30);
      final newState = state.updateCountdown(25);

      expect(newState.countdownSeconds, 25);
      expect(newState.isCountingDown, true);
    });

    test('cancelCountdown resets countdown state', () {
      const state = AlarmState(isCountingDown: true, countdownSeconds: 15);
      final newState = state.cancelCountdown();

      expect(newState.isCountingDown, false);
      expect(newState.countdownSeconds, 0);
    });

    test('activate sets alarm to active state', () {
      const state = AlarmState();
      final newState = state.activate(AlarmMode.aggressive);

      expect(newState.isActive, true);
      expect(newState.isTriggered, false);
      expect(newState.isCountingDown, false);
      expect(newState.countdownSeconds, 0);
      expect(newState.mode, AlarmMode.aggressive);
      expect(newState.activatedAt, isNotNull);
    });

    test('deactivate resets alarm to inactive state', () {
      final activatedTime = DateTime.now().subtract(const Duration(hours: 1));
      final triggeredTime = DateTime.now().subtract(const Duration(minutes: 5));
      final state = AlarmState(
        isActive: true,
        isTriggered: true,
        activatedAt: activatedTime,
        lastTriggeredAt: triggeredTime,
        triggerCount: 3,
        mode: AlarmMode.aggressive,
      );

      final newState = state.deactivate();

      expect(newState.isActive, false);
      expect(newState.isTriggered, false);
      expect(newState.isCountingDown, false);
      expect(newState.activatedAt, null);
      expect(newState.lastTriggeredAt, triggeredTime); // preserved
      expect(newState.triggerCount, 3); // preserved
      expect(newState.mode, AlarmMode.aggressive); // preserved
    });

    test('trigger sets triggered state and increments count', () {
      const state = AlarmState(isActive: true, triggerCount: 2);
      final newState = state.trigger();

      expect(newState.isTriggered, true);
      expect(newState.triggerCount, 3);
      expect(newState.lastTriggeredAt, isNotNull);
    });

    test('acknowledge clears triggered state', () {
      const state = AlarmState(isTriggered: true);
      final newState = state.acknowledge();

      expect(newState.isTriggered, false);
    });

    test('activeDuration returns null when not activated', () {
      const state = AlarmState();
      expect(state.activeDuration, null);
    });

    test('activeDuration calculates time since activation', () {
      final activatedTime =
          DateTime.now().subtract(const Duration(minutes: 30));
      final state = AlarmState(activatedAt: activatedTime);

      final duration = state.activeDuration;
      expect(duration, isNotNull);
      expect(duration!.inMinutes, greaterThanOrEqualTo(29));
      expect(duration.inMinutes, lessThanOrEqualTo(31));
    });

    test('timeSinceLastTrigger returns null when never triggered', () {
      const state = AlarmState();
      expect(state.timeSinceLastTrigger, null);
    });

    test('timeSinceLastTrigger calculates time since last trigger', () {
      final triggeredTime =
          DateTime.now().subtract(const Duration(seconds: 45));
      final state = AlarmState(lastTriggeredAt: triggeredTime);

      final duration = state.timeSinceLastTrigger;
      expect(duration, isNotNull);
      expect(duration!.inSeconds, greaterThanOrEqualTo(44));
      expect(duration.inSeconds, lessThanOrEqualTo(46));
    });

    test('toString returns readable representation', () {
      const state = AlarmState(
        isActive: true,
        isTriggered: false,
        triggerCount: 3,
        mode: AlarmMode.stealth,
      );

      final str = state.toString();
      expect(str, contains('isActive: true'));
      expect(str, contains('isTriggered: false'));
      expect(str, contains('triggerCount: 3'));
      expect(str, contains('AlarmMode.stealth'));
    });
  });

  group('AlarmMode Extension Tests', () {
    test('display names are correct', () {
      expect(AlarmMode.standard.displayName, 'Standard');
      expect(AlarmMode.stealth.displayName, 'Stealth');
      expect(AlarmMode.aggressive.displayName, 'Aggressive');
    });

    test('descriptions are provided', () {
      expect(AlarmMode.standard.description, isNotEmpty);
      expect(AlarmMode.stealth.description, isNotEmpty);
      expect(AlarmMode.aggressive.description, isNotEmpty);
      expect(AlarmMode.stealth.description, contains('Silent'));
    });

    test('bark intensity values are correct', () {
      expect(AlarmMode.standard.barkIntensity, 1.0);
      expect(AlarmMode.stealth.barkIntensity, 0.0);
      expect(AlarmMode.aggressive.barkIntensity, 1.5);
    });

    test('delayed response settings are correct', () {
      expect(AlarmMode.standard.hasDelayedResponse, true);
      expect(AlarmMode.stealth.hasDelayedResponse, true);
      expect(AlarmMode.aggressive.hasDelayedResponse, false);
    });
  });
}
