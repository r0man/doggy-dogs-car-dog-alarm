/// Represents the state of the car alarm
class AlarmState {
  final bool isActive;
  final bool isTriggered;
  final DateTime? activatedAt;
  final DateTime? lastTriggeredAt;
  final int triggerCount;
  final AlarmMode mode;

  const AlarmState({
    this.isActive = false,
    this.isTriggered = false,
    this.activatedAt,
    this.lastTriggeredAt,
    this.triggerCount = 0,
    this.mode = AlarmMode.standard,
  });

  AlarmState copyWith({
    bool? isActive,
    bool? isTriggered,
    DateTime? activatedAt,
    DateTime? lastTriggeredAt,
    int? triggerCount,
    AlarmMode? mode,
  }) {
    return AlarmState(
      isActive: isActive ?? this.isActive,
      isTriggered: isTriggered ?? this.isTriggered,
      activatedAt: activatedAt ?? this.activatedAt,
      lastTriggeredAt: lastTriggeredAt ?? this.lastTriggeredAt,
      triggerCount: triggerCount ?? this.triggerCount,
      mode: mode ?? this.mode,
    );
  }

  /// Create state for activated alarm
  AlarmState activate(AlarmMode alarmMode) {
    return copyWith(
      isActive: true,
      isTriggered: false,
      activatedAt: DateTime.now(),
      mode: alarmMode,
    );
  }

  /// Create state for deactivated alarm
  AlarmState deactivate() {
    return copyWith(
      isActive: false,
      isTriggered: false,
      activatedAt: null,
    );
  }

  /// Create state for triggered alarm
  AlarmState trigger() {
    return copyWith(
      isTriggered: true,
      lastTriggeredAt: DateTime.now(),
      triggerCount: triggerCount + 1,
    );
  }

  /// Create state for acknowledged (silenced) alarm
  AlarmState acknowledge() {
    return copyWith(isTriggered: false);
  }

  /// Get duration since alarm was activated
  Duration? get activeDuration {
    if (activatedAt == null) return null;
    return DateTime.now().difference(activatedAt!);
  }

  /// Get duration since last trigger
  Duration? get timeSinceLastTrigger {
    if (lastTriggeredAt == null) return null;
    return DateTime.now().difference(lastTriggeredAt!);
  }

  @override
  String toString() {
    return 'AlarmState(isActive: $isActive, isTriggered: $isTriggered, '
        'triggerCount: $triggerCount, mode: $mode)';
  }
}

/// Alarm operating mode
enum AlarmMode {
  standard,
  stealth,
  aggressive,
}

extension AlarmModeExtension on AlarmMode {
  String get displayName {
    switch (this) {
      case AlarmMode.standard:
        return 'Standard';
      case AlarmMode.stealth:
        return 'Stealth';
      case AlarmMode.aggressive:
        return 'Aggressive';
    }
  }

  String get description {
    switch (this) {
      case AlarmMode.standard:
        return 'Normal alarm response with escalating barks';
      case AlarmMode.stealth:
        return 'Silent notifications only, no audible barking';
      case AlarmMode.aggressive:
        return 'Immediate loud barking on any motion';
    }
  }

  /// Get bark intensity multiplier for this mode
  double get barkIntensity {
    switch (this) {
      case AlarmMode.standard:
        return 1.0;
      case AlarmMode.stealth:
        return 0.0;
      case AlarmMode.aggressive:
        return 1.5;
    }
  }

  /// Should the alarm delay response to verify threat?
  bool get hasDelayedResponse {
    switch (this) {
      case AlarmMode.standard:
        return true;
      case AlarmMode.stealth:
        return true;
      case AlarmMode.aggressive:
        return false;
    }
  }
}
