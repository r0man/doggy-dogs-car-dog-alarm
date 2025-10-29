import 'package:flutter/foundation.dart';

/// Application settings model
@immutable
class AppSettings {
  // Alarm settings
  final int countdownDuration; // seconds (15-120)
  final String sensitivityLevel; // 'low', 'medium', 'high', 'veryHigh'
  final double barkVolume; // 0.0 to 1.0

  // Notifications
  final bool notificationsEnabled;

  // Battery optimization
  final bool batteryOptimizationEnabled;

  const AppSettings({
    this.countdownDuration = 30,
    this.sensitivityLevel = 'medium',
    this.barkVolume = 0.8,
    this.notificationsEnabled = true,
    this.batteryOptimizationEnabled = true,
  });

  AppSettings copyWith({
    int? countdownDuration,
    String? sensitivityLevel,
    double? barkVolume,
    bool? notificationsEnabled,
    bool? batteryOptimizationEnabled,
  }) {
    return AppSettings(
      countdownDuration: countdownDuration ?? this.countdownDuration,
      sensitivityLevel: sensitivityLevel ?? this.sensitivityLevel,
      barkVolume: barkVolume ?? this.barkVolume,
      notificationsEnabled: notificationsEnabled ?? this.notificationsEnabled,
      batteryOptimizationEnabled: batteryOptimizationEnabled ?? this.batteryOptimizationEnabled,
    );
  }

  /// Convert to JSON for storage
  Map<String, dynamic> toJson() {
    return {
      'countdownDuration': countdownDuration,
      'sensitivityLevel': sensitivityLevel,
      'barkVolume': barkVolume,
      'notificationsEnabled': notificationsEnabled,
      'batteryOptimizationEnabled': batteryOptimizationEnabled,
    };
  }

  /// Create from JSON
  factory AppSettings.fromJson(Map<String, dynamic> json) {
    return AppSettings(
      countdownDuration: json['countdownDuration'] as int? ?? 30,
      sensitivityLevel: json['sensitivityLevel'] as String? ?? 'medium',
      barkVolume: (json['barkVolume'] as num?)?.toDouble() ?? 0.8,
      notificationsEnabled: json['notificationsEnabled'] as bool? ?? true,
      batteryOptimizationEnabled: json['batteryOptimizationEnabled'] as bool? ?? true,
    );
  }

  /// Validate countdown duration
  bool isValidCountdownDuration() {
    return countdownDuration >= 15 && countdownDuration <= 120;
  }

  /// Validate bark volume
  bool isValidBarkVolume() {
    return barkVolume >= 0.0 && barkVolume <= 1.0;
  }

  /// Validate sensitivity level
  bool isValidSensitivityLevel() {
    return ['low', 'medium', 'high', 'veryHigh'].contains(sensitivityLevel);
  }

  /// Validate all settings
  bool isValid() {
    return isValidCountdownDuration() &&
           isValidBarkVolume() &&
           isValidSensitivityLevel();
  }

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is AppSettings &&
          runtimeType == other.runtimeType &&
          countdownDuration == other.countdownDuration &&
          sensitivityLevel == other.sensitivityLevel &&
          barkVolume == other.barkVolume &&
          notificationsEnabled == other.notificationsEnabled &&
          batteryOptimizationEnabled == other.batteryOptimizationEnabled;

  @override
  int get hashCode =>
      countdownDuration.hashCode ^
      sensitivityLevel.hashCode ^
      barkVolume.hashCode ^
      notificationsEnabled.hashCode ^
      batteryOptimizationEnabled.hashCode;

  @override
  String toString() {
    return 'AppSettings('
        'countdownDuration: $countdownDuration, '
        'sensitivityLevel: $sensitivityLevel, '
        'barkVolume: $barkVolume, '
        'notificationsEnabled: $notificationsEnabled, '
        'batteryOptimizationEnabled: $batteryOptimizationEnabled'
        ')';
  }
}
