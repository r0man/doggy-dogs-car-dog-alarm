import 'dart:async';

/// Callback function invoked on each countdown tick
typedef CountdownTickCallback = void Function(int remainingSeconds);

/// Callback function invoked when countdown completes
typedef CountdownCompleteCallback = void Function();

/// Pure countdown timer logic without platform dependencies
/// This class is fully testable with fake async
class AlarmTimer {
  Timer? _timer;
  int? _remainingSeconds;
  CountdownTickCallback? _onTick;
  CountdownCompleteCallback? _onComplete;

  /// Check if timer is currently running
  bool get isRunning => _timer != null && _timer!.isActive;

  /// Get remaining seconds (null if not running)
  int? get remainingSeconds => _remainingSeconds;

  /// Start a countdown timer
  /// [duration] - total countdown duration in seconds
  /// [onTick] - called every second with remaining seconds
  /// [onComplete] - called when countdown reaches zero
  void start({
    required int duration,
    required CountdownTickCallback onTick,
    required CountdownCompleteCallback onComplete,
  }) {
    if (isRunning) {
      throw StateError('Timer is already running');
    }

    if (duration <= 0) {
      throw ArgumentError('Duration must be positive');
    }

    _remainingSeconds = duration;
    _onTick = onTick;
    _onComplete = onComplete;

    _timer = Timer.periodic(const Duration(seconds: 1), _handleTick);
  }

  /// Handle each timer tick
  void _handleTick(Timer timer) {
    if (_remainingSeconds == null) {
      timer.cancel();
      return;
    }

    _remainingSeconds = _remainingSeconds! - 1;

    if (_remainingSeconds! <= 0) {
      timer.cancel();
      _timer = null;
      final callback = _onComplete;
      _onComplete = null;
      _onTick = null;
      _remainingSeconds = null;
      callback?.call();
    } else {
      _onTick?.call(_remainingSeconds!);
    }
  }

  /// Stop the countdown timer
  void stop() {
    _timer?.cancel();
    _timer = null;
    _remainingSeconds = null;
    _onTick = null;
    _onComplete = null;
  }

  /// Dispose of timer resources
  void dispose() {
    stop();
  }
}
