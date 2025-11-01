import '../models/alarm_state.dart';
import '../models/sensor_data.dart';
import '../models/dog.dart';

/// Result of evaluating whether motion should trigger an alarm
class TriggerDecision {
  final bool shouldTrigger;
  final TriggerReason reason;
  final double effectiveIntensity;

  const TriggerDecision({
    required this.shouldTrigger,
    required this.reason,
    required this.effectiveIntensity,
  });

  const TriggerDecision.trigger(this.reason, this.effectiveIntensity)
      : shouldTrigger = true;

  const TriggerDecision.verify(this.effectiveIntensity)
      : shouldTrigger = false,
        reason = TriggerReason.needsVerification;

  const TriggerDecision.ignore(this.effectiveIntensity)
      : shouldTrigger = false,
        reason = TriggerReason.belowThreshold;
}

/// Reason for trigger decision
enum TriggerReason {
  belowThreshold,
  needsVerification,
  aggressiveMode,
  highIntensity,
  verifiedThreat,
}

/// Pure business logic for alarm trigger conditions and threat verification
/// This class is fully testable without any platform dependencies
class AlarmConditions {
  static const verificationWindow = Duration(seconds: 3);
  static const motionsToConfirm = 2;
  static const immediateThreshold = 0.8;

  final AlarmSensitivity sensitivity;
  final Dog guardDog;

  const AlarmConditions({
    required this.sensitivity,
    required this.guardDog,
  });

  /// Evaluate whether a motion event should trigger the alarm
  TriggerDecision evaluateMotion(MotionEvent event, AlarmMode mode) {
    // Check if motion exceeds sensitivity threshold
    if (!event.shouldTriggerAlarm(sensitivity)) {
      return TriggerDecision.ignore(event.intensity);
    }

    // Apply dog effectiveness modifier
    final effectiveIntensity = _calculateEffectiveIntensity(event.intensity);

    // Aggressive mode: trigger immediately
    if (mode == AlarmMode.aggressive) {
      return TriggerDecision.trigger(
        TriggerReason.aggressiveMode,
        effectiveIntensity,
      );
    }

    // If intensity is very high, trigger immediately regardless of mode
    if (effectiveIntensity > immediateThreshold) {
      return TriggerDecision.trigger(
        TriggerReason.highIntensity,
        effectiveIntensity,
      );
    }

    // Standard/Stealth mode: verify threat first
    if (mode.hasDelayedResponse) {
      return TriggerDecision.verify(effectiveIntensity);
    }

    // Default: ignore
    return TriggerDecision.ignore(effectiveIntensity);
  }

  /// Calculate effective intensity with dog effectiveness modifier
  double _calculateEffectiveIntensity(double intensity) {
    return intensity * (guardDog.effectiveness / 100);
  }

  /// Evaluate a list of recent motions to determine if threat is verified
  bool isVerifiedThreat(List<MotionEvent> recentMotions) {
    if (recentMotions.length < motionsToConfirm) {
      return false;
    }

    // Check if motions are within verification window
    final now = DateTime.now();
    final cutoff = now.subtract(verificationWindow);

    final validMotions =
        recentMotions.where((m) => m.timestamp.isAfter(cutoff)).toList();

    return validMotions.length >= motionsToConfirm;
  }

  /// Filter out motions that are outside the verification window
  List<MotionEvent> filterRecentMotions(List<MotionEvent> motions) {
    final cutoff = DateTime.now().subtract(verificationWindow);
    return motions.where((m) => m.timestamp.isAfter(cutoff)).toList();
  }

  /// Create a new instance with different settings
  AlarmConditions copyWith({
    AlarmSensitivity? sensitivity,
    Dog? guardDog,
  }) {
    return AlarmConditions(
      sensitivity: sensitivity ?? this.sensitivity,
      guardDog: guardDog ?? this.guardDog,
    );
  }
}
