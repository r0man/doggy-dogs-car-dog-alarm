import 'dart:async';
import 'package:flutter/foundation.dart';
import '../models/alarm_state.dart';
import '../models/dog.dart';
import '../models/dog_animation_state.dart';

/// Controls dog animation state based on alarm state, mood, and user interactions
class DogAnimationController extends ChangeNotifier {
  DogAnimationState _currentState = DogAnimationState.idle;
  DogAnimationState? _previousState;
  Timer? _stateTimer;
  bool _isTransitioning = false;
  bool _isDisposed = false;

  /// Current animation state
  DogAnimationState get currentState => _currentState;

  /// Previous animation state (useful for transitions)
  DogAnimationState? get previousState => _previousState;

  /// Whether currently transitioning between states
  bool get isTransitioning => _isTransitioning;

  /// Dispose of resources
  @override
  void dispose() {
    _isDisposed = true;
    _stateTimer?.cancel();
    super.dispose();
  }

  /// Update animation based on alarm state
  /// Alarm state changes are authoritative and override mood-based animations
  void updateFromAlarmState(AlarmState alarmState) {
    if (alarmState.isTriggered) {
      // Alarm triggered - bark!
      _transitionTo(DogAnimationState.barking, force: true);
    } else if (alarmState.isCountingDown) {
      // Countdown active - be alert
      _transitionTo(DogAnimationState.alert, force: true);
    } else if (alarmState.isActive) {
      // Alarm active but not triggered - vigilant
      _transitionTo(DogAnimationState.alert, force: true);
    } else {
      // No alarm - return to idle (force to ensure transition)
      _transitionTo(DogAnimationState.idle, force: true);
    }
  }

  /// Update animation based on dog mood
  /// Only updates if not currently in alarm-related state
  void updateFromMood(DogMood mood) {
    // Don't override alarm-driven states (barking, alert)
    // These should only be changed by updateFromAlarmState()
    if (_currentState == DogAnimationState.barking || _currentState == DogAnimationState.alert) {
      debugPrint(
        'Animation: Ignoring mood update to ${mood.name} '
        '(currently in alarm state: ${_currentState.displayName})',
      );
      return;
    }

    final newState = _mapMoodToAnimation(mood);
    // Allow mood transitions without strict priority checking
    _transitionTo(newState, force: true);
  }

  /// Trigger a one-time animation (eating, playing)
  /// Returns to previous state when complete
  Future<void> playOnce(DogAnimationState state) async {
    if (!state.isLooping) {
      _transitionTo(state);

      // Schedule return to idle after animation completes
      _stateTimer?.cancel();
      _stateTimer = Timer(
        Duration(milliseconds: (state.duration * 1000).toInt()),
        () => _transitionTo(DogAnimationState.idle),
      );
    }
  }

  /// Force transition to a specific state (e.g., for testing or special events)
  void forceState(DogAnimationState state) {
    _transitionTo(state, force: true);
  }

  /// Map dog mood to appropriate animation state
  DogAnimationState _mapMoodToAnimation(DogMood mood) {
    switch (mood) {
      case DogMood.happy:
      case DogMood.excited:
        return DogAnimationState.happy;
      case DogMood.alert:
        return DogAnimationState.alert;
      case DogMood.worried:
      case DogMood.sad:
        return DogAnimationState.sad;
      case DogMood.sleeping:
        return DogAnimationState.sleeping;
      case DogMood.grumpy:
        return DogAnimationState.sad;
      case DogMood.content:
      default:
        return DogAnimationState.idle;
    }
  }

  /// Transition to a new animation state
  void _transitionTo(DogAnimationState newState, {bool force = false}) {
    // Don't transition to same state unless forced
    if (_currentState == newState && !force) {
      return;
    }

    // Check priority - don't interrupt higher priority animations unless forced
    if (!force && newState.priority < _currentState.priority) {
      debugPrint(
        'Animation: Ignoring transition to ${newState.displayName} '
        '(priority ${newState.priority} < ${_currentState.priority})',
      );
      return;
    }

    debugPrint(
      'Animation: Transitioning from ${_currentState.displayName} '
      'to ${newState.displayName}',
    );

    _isTransitioning = true;
    _previousState = _currentState;
    _currentState = newState;

    // Clear any pending state changes
    _stateTimer?.cancel();

    if (!_isDisposed) {
      notifyListeners();
    }

    // Mark transition complete after a brief delay
    Future.delayed(const Duration(milliseconds: 300), () {
      if (!_isDisposed) {
        _isTransitioning = false;
        notifyListeners();
      }
    });
  }

  /// Get recommended transition duration based on state change
  Duration getTransitionDuration(
    DogAnimationState from,
    DogAnimationState to,
  ) {
    // Urgent transitions (e.g., idle → barking) should be fast
    if (to == DogAnimationState.barking) {
      return const Duration(milliseconds: 200);
    }

    // Relaxing transitions (e.g., alert → sleeping) can be slow
    if (to == DogAnimationState.sleeping) {
      return const Duration(milliseconds: 800);
    }

    // Default transition
    return const Duration(milliseconds: 400);
  }

  /// Check if a state transition is allowed
  bool canTransitionTo(DogAnimationState newState) {
    // Barking can always interrupt (highest priority)
    if (newState == DogAnimationState.barking) {
      return true;
    }

    // Check priority
    return newState.priority >= _currentState.priority;
  }
}
