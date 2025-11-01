import 'package:flutter_test/flutter_test.dart';
import 'package:doggy_dogs_car_alarm/services/alarm_timer.dart';

void main() {
  group('AlarmTimer Tests', () {
    late AlarmTimer timer;

    setUp(() {
      timer = AlarmTimer();
    });

    tearDown(() {
      timer.dispose();
    });

    test('initial state is not running', () {
      expect(timer.isRunning, false);
      expect(timer.remainingSeconds, null);
    });

    group('start', () {
      test('starts countdown with correct duration', () async {
        final ticks = <int>[];
        var completed = false;

        timer.start(
          duration: 3,
          onTick: ticks.add,
          onComplete: () => completed = true,
        );

        expect(timer.isRunning, true);
        expect(timer.remainingSeconds, 3);

        // Wait for countdown to complete
        await Future.delayed(const Duration(milliseconds: 3100));

        expect(ticks, [2, 1]);
        expect(completed, true);
        expect(timer.isRunning, false);
        expect(timer.remainingSeconds, null);
      });

      test('calls onTick on each second', () async {
        final ticks = <int>[];

        timer.start(
          duration: 5,
          onTick: ticks.add,
          onComplete: () {},
        );

        await Future.delayed(const Duration(milliseconds: 5100));

        expect(ticks, [4, 3, 2, 1]);
      });

      test('calls onComplete when countdown reaches zero', () async {
        var completed = false;

        timer.start(
          duration: 2,
          onTick: (_) {},
          onComplete: () => completed = true,
        );

        expect(completed, false);
        await Future.delayed(const Duration(milliseconds: 2100));

        expect(completed, true);
      });

      test('throws StateError if timer is already running', () {
        timer.start(
          duration: 5,
          onTick: (_) {},
          onComplete: () {},
        );

        expect(
          () => timer.start(
            duration: 3,
            onTick: (_) {},
            onComplete: () {},
          ),
          throwsStateError,
        );
      });

      test('throws ArgumentError if duration is zero', () {
        expect(
          () => timer.start(
            duration: 0,
            onTick: (_) {},
            onComplete: () {},
          ),
          throwsArgumentError,
        );
      });

      test('throws ArgumentError if duration is negative', () {
        expect(
          () => timer.start(
            duration: -5,
            onTick: (_) {},
            onComplete: () {},
          ),
          throwsArgumentError,
        );
      });

      test('updates remainingSeconds during countdown', () async {
        timer.start(
          duration: 3,
          onTick: (_) {},
          onComplete: () {},
        );

        expect(timer.remainingSeconds, 3);

        await Future.delayed(const Duration(milliseconds: 1100));
        expect(timer.remainingSeconds, 2);

        await Future.delayed(const Duration(milliseconds: 1000));
        expect(timer.remainingSeconds, 1);
      });
    });

    group('stop', () {
      test('stops running countdown', () async {
        final ticks = <int>[];
        var completed = false;

        timer.start(
          duration: 5,
          onTick: ticks.add,
          onComplete: () => completed = true,
        );

        await Future.delayed(const Duration(milliseconds: 2100));
        timer.stop();

        expect(timer.isRunning, false);
        expect(timer.remainingSeconds, null);

        // Wait to ensure no more ticks occur
        await Future.delayed(const Duration(milliseconds: 2000));

        expect(ticks.length, 2); // Only ticked twice before stop
        expect(completed, false); // Never completed
      });

      test('can be called when timer is not running (no-op)', () {
        expect(() => timer.stop(), returnsNormally);
      });

      test('clears callbacks', () async {
        var callbackCalled = false;

        timer.start(
          duration: 3,
          onTick: (_) => callbackCalled = true,
          onComplete: () => callbackCalled = true,
        );

        timer.stop();

        await Future.delayed(const Duration(milliseconds: 3100));
        expect(callbackCalled, false);
      });
    });

    group('dispose', () {
      test('stops timer and cleans up', () async {
        final ticks = <int>[];

        timer.start(
          duration: 5,
          onTick: ticks.add,
          onComplete: () {},
        );

        await Future.delayed(const Duration(milliseconds: 1100));
        timer.dispose();

        expect(timer.isRunning, false);

        // Wait to ensure no more ticks occur
        await Future.delayed(const Duration(milliseconds: 2000));
        expect(ticks.length, 1);
      });
    });

    group('edge cases', () {
      test('countdown of 1 second completes correctly', () async {
        var completed = false;
        final ticks = <int>[];

        timer.start(
          duration: 1,
          onTick: ticks.add,
          onComplete: () => completed = true,
        );

        await Future.delayed(const Duration(milliseconds: 1100));

        expect(ticks.isEmpty, true); // No ticks, goes straight to complete
        expect(completed, true);
      });

      test('can start new countdown after previous completes', () async {
        var firstComplete = false;
        var secondComplete = false;

        timer.start(
          duration: 1,
          onTick: (_) {},
          onComplete: () => firstComplete = true,
        );

        await Future.delayed(const Duration(milliseconds: 1100));
        expect(firstComplete, true);

        timer.start(
          duration: 1,
          onTick: (_) {},
          onComplete: () => secondComplete = true,
        );

        await Future.delayed(const Duration(milliseconds: 1100));
        expect(secondComplete, true);
      });

      test('can start new countdown after stop', () async {
        timer.start(
          duration: 5,
          onTick: (_) {},
          onComplete: () {},
        );

        await Future.delayed(const Duration(milliseconds: 1100));
        timer.stop();

        var completed = false;
        timer.start(
          duration: 2,
          onTick: (_) {},
          onComplete: () => completed = true,
        );

        await Future.delayed(const Duration(milliseconds: 2100));
        expect(completed, true);
      });

      test('multiple stops are safe', () {
        timer.start(
          duration: 5,
          onTick: (_) {},
          onComplete: () {},
        );

        expect(() {
          timer.stop();
          timer.stop();
          timer.stop();
        }, returnsNormally);
      });

      test('multiple disposes are safe', () {
        timer.start(
          duration: 5,
          onTick: (_) {},
          onComplete: () {},
        );

        expect(() {
          timer.dispose();
          timer.dispose();
        }, returnsNormally);
      });
    });

    group('accuracy', () {
      test('countdown duration is approximately correct', () async {
        var completed = false;
        final startTime = DateTime.now();

        timer.start(
          duration: 3,
          onTick: (_) {},
          onComplete: () {
            completed = true;
          },
        );

        while (!completed) {
          await Future.delayed(const Duration(milliseconds: 100));
        }

        final elapsed = DateTime.now().difference(startTime);

        // Should complete in approximately 3 seconds (with some tolerance)
        expect(elapsed.inMilliseconds, greaterThan(2900));
        expect(elapsed.inMilliseconds, lessThan(3200));
      });
    });
  });
}
