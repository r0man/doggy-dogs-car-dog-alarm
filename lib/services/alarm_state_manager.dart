import 'dart:async';
import '../models/alarm_state.dart';

/// Pure business logic for managing alarm state transitions
/// This class is fully testable without any platform dependencies
class AlarmStateManager {
  final _stateController = StreamController<AlarmState>.broadcast();
  AlarmState _currentState = const AlarmState();

  /// Stream of alarm state changes
  Stream<AlarmState> get stateStream => _stateController.stream;

  /// Current alarm state
  AlarmState get currentState => _currentState;

  /// Check if alarm can be activated
  bool canStartActivation() {
    return !_currentState.isActive && !_currentState.isCountingDown;
  }

  /// Start activation countdown
  AlarmState startCountdown(AlarmMode mode, int countdownDuration) {
    if (!canStartActivation()) {
      throw StateError(
        'Cannot start countdown: alarm is already active or counting down',
      );
    }

    _currentState = _currentState.startCountdown(mode, countdownDuration);
    _stateController.add(_currentState);
    return _currentState;
  }

  /// Update countdown timer
  AlarmState updateCountdown(int remainingSeconds) {
    if (!_currentState.isCountingDown) {
      throw StateError('Cannot update countdown: countdown is not active');
    }

    _currentState = _currentState.updateCountdown(remainingSeconds);
    _stateController.add(_currentState);
    return _currentState;
  }

  /// Check if countdown can be completed (reached zero)
  bool canCompleteCountdown() {
    return _currentState.isCountingDown;
  }

  /// Complete activation after countdown
  AlarmState completeActivation(AlarmMode mode) {
    if (!canCompleteCountdown()) {
      throw StateError('Cannot complete activation: countdown is not active');
    }

    _currentState = _currentState.activate(mode);
    _stateController.add(_currentState);
    return _currentState;
  }

  /// Check if countdown can be cancelled
  bool canCancelCountdown() {
    return _currentState.isCountingDown;
  }

  /// Cancel activation countdown
  AlarmState cancelCountdown() {
    if (!canCancelCountdown()) {
      throw StateError('Cannot cancel countdown: countdown is not active');
    }

    _currentState = _currentState.cancelCountdown();
    _stateController.add(_currentState);
    return _currentState;
  }

  /// Check if alarm can be deactivated
  bool canDeactivate() {
    return _currentState.isActive;
  }

  /// Deactivate the alarm
  AlarmState deactivate() {
    if (!canDeactivate()) {
      throw StateError('Cannot deactivate: alarm is not active');
    }

    _currentState = _currentState.deactivate();
    _stateController.add(_currentState);
    return _currentState;
  }

  /// Check if alarm can be triggered
  bool canTrigger() {
    return _currentState.isActive && !_currentState.isTriggered;
  }

  /// Trigger the alarm
  AlarmState trigger() {
    if (!canTrigger()) {
      return _currentState; // Already triggered, no-op
    }

    _currentState = _currentState.trigger();
    _stateController.add(_currentState);
    return _currentState;
  }

  /// Check if alarm can be acknowledged
  bool canAcknowledge() {
    return _currentState.isTriggered;
  }

  /// Acknowledge and silence a triggered alarm
  AlarmState acknowledge() {
    if (!canAcknowledge()) {
      throw StateError('Cannot acknowledge: alarm is not triggered');
    }

    _currentState = _currentState.acknowledge();
    _stateController.add(_currentState);
    return _currentState;
  }

  /// Set state directly (for restoration from persistence)
  void setState(AlarmState state) {
    _currentState = state;
    _stateController.add(_currentState);
  }

  /// Dispose of resources
  void dispose() {
    _stateController.close();
  }
}
