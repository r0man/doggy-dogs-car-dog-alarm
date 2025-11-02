import 'package:flutter/material.dart';
import '../models/alarm_state.dart';
import '../models/sensor_data.dart';

/// View model for alarm screen that maps state to UI configuration
class AlarmScreenViewModel {
  final AlarmState alarmState;
  final AlarmSensitivity sensitivity;

  const AlarmScreenViewModel({
    required this.alarmState,
    required this.sensitivity,
  });

  /// Get display configuration for the current alarm state
  AlarmDisplayConfig get displayConfig {
    if (alarmState.isCountingDown) {
      return AlarmDisplayConfig.countdown(
        countdownSeconds: alarmState.countdownSeconds,
      );
    } else if (alarmState.isTriggered) {
      return AlarmDisplayConfig.triggered(
        mode: alarmState.mode,
        triggerCount: alarmState.triggerCount,
      );
    } else if (alarmState.isActive) {
      return AlarmDisplayConfig.active(
        mode: alarmState.mode,
        sensitivity: sensitivity,
        activeDuration: alarmState.activeDuration,
        triggerCount: alarmState.triggerCount,
      );
    } else {
      return const AlarmDisplayConfig.inactive();
    }
  }

  /// Check if mode selection should be shown
  bool get shouldShowModeSelection =>
      !alarmState.isActive && !alarmState.isCountingDown;

  /// Check if sensitivity selection should be shown
  bool get shouldShowSensitivitySelection =>
      !alarmState.isActive && !alarmState.isCountingDown;

  /// Format duration for display
  static String formatDuration(Duration duration) {
    if (duration.inHours > 0) {
      return '${duration.inHours}h ${duration.inMinutes.remainder(60)}m';
    } else if (duration.inMinutes > 0) {
      return '${duration.inMinutes}m ${duration.inSeconds.remainder(60)}s';
    } else {
      return '${duration.inSeconds}s';
    }
  }
}

/// Configuration for alarm display UI
class AlarmDisplayConfig {
  final AlarmDisplayState state;
  final Color backgroundColor;
  final Color foregroundColor;
  final IconData icon;
  final String statusText;
  final String? modeText;
  final String? sensitivityText;
  final String? durationText;
  final String? countdownText;
  final int? triggerCount;
  final ActionButtonConfig buttonConfig;

  /// Configuration for inactive state
  const AlarmDisplayConfig.inactive()
      : state = AlarmDisplayState.inactive,
        backgroundColor = const Color(0xFFF5F5F5), // Colors.grey.shade50
        foregroundColor = Colors.grey,
        icon = Icons.security_outlined,
        statusText = 'GUARD DOG READY',
        modeText = 'Tap below to activate car alarm protection',
        sensitivityText = null,
        durationText = null,
        countdownText = null,
        triggerCount = null,
        buttonConfig = const ActionButtonConfig(
          text: 'ACTIVATE GUARD DOG',
          icon: Icons.security,
          backgroundColor: Colors.green,
          foregroundColor: Colors.white,
          action: AlarmAction.activate,
        );

  /// Configuration for countdown state
  AlarmDisplayConfig.countdown({
    required int countdownSeconds,
  })  : state = AlarmDisplayState.countdown,
        backgroundColor = const Color(0xFFFFF3E0), // Colors.orange.shade50
        foregroundColor = Colors.orange,
        icon = Icons.timer,
        statusText = 'ACTIVATING IN',
        modeText = null,
        sensitivityText = null,
        durationText = null,
        countdownText = '$countdownSeconds',
        triggerCount = null,
        buttonConfig = const ActionButtonConfig(
          text: 'CANCEL ACTIVATION',
          icon: Icons.cancel,
          backgroundColor: Colors.orange,
          foregroundColor: Colors.white,
          action: AlarmAction.cancelCountdown,
        );

  /// Configuration for active state
  AlarmDisplayConfig.active({
    required AlarmMode mode,
    required AlarmSensitivity sensitivity,
    required Duration? activeDuration,
    required int triggerCount,
  })  : state = AlarmDisplayState.active,
        backgroundColor = const Color(0xFFE8F5E9), // Colors.green.shade50
        foregroundColor = Colors.green,
        icon = Icons.security,
        statusText = 'GUARD DOG ACTIVE',
        modeText = 'Mode: ${mode.displayName}',
        sensitivityText = 'Sensitivity: ${sensitivity.name}',
        durationText = activeDuration != null
            ? 'Active for: ${AlarmScreenViewModel.formatDuration(activeDuration)}'
            : null,
        countdownText = null,
        triggerCount = triggerCount > 0 ? triggerCount : null,
        buttonConfig = const ActionButtonConfig(
          text: 'DEACTIVATE ALARM',
          icon: Icons.stop,
          backgroundColor: Colors.red,
          foregroundColor: Colors.white,
          action: AlarmAction.deactivate,
        );

  /// Configuration for triggered state
  AlarmDisplayConfig.triggered({
    required AlarmMode mode,
    required this.triggerCount,
  })  : state = AlarmDisplayState.triggered,
        backgroundColor = const Color(0xFFFFEBEE), // Colors.red.shade50
        foregroundColor = Colors.red,
        icon = Icons.warning_amber_rounded,
        statusText = 'ALARM TRIGGERED!',
        modeText = null,
        sensitivityText = null,
        durationText = null,
        countdownText = null,
        buttonConfig = const ActionButtonConfig(
          text: 'ACKNOWLEDGE',
          icon: Icons.check_circle,
          backgroundColor: Colors.orange,
          foregroundColor: Colors.white,
          action: AlarmAction.acknowledge,
        );
}

/// Possible states for alarm display
enum AlarmDisplayState {
  inactive,
  countdown,
  active,
  triggered,
}

/// Configuration for action button
class ActionButtonConfig {
  final String text;
  final IconData icon;
  final Color backgroundColor;
  final Color foregroundColor;
  final AlarmAction action;

  const ActionButtonConfig({
    required this.text,
    required this.icon,
    required this.backgroundColor,
    required this.foregroundColor,
    required this.action,
  });
}

/// Actions that can be triggered from the UI
enum AlarmAction {
  activate,
  deactivate,
  cancelCountdown,
  acknowledge,
  recalibrate,
}
