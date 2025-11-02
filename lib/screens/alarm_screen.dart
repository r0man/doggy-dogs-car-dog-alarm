import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import '../models/alarm_state.dart';
import '../models/sensor_data.dart';
import '../services/alarm_service.dart';
import '../services/sensor_detection_service.dart';
import '../services/app_settings_service.dart';
import '../widgets/unlock_dialog.dart';
import 'alarm_screen_view_model.dart';

/// User feedback messages for alarm actions
class AlarmFeedbackMessages {
  static const activating = 'Activating Guard Dog...';
  static const cancelled = 'Activation cancelled';
  static const acknowledged = 'Alarm acknowledged';
  static const recalibrated = 'Sensors recalibrated';
}

/// Feedback types for snackbar styling
enum FeedbackType {
  /// Warning/pending state (orange/amber)
  warning,

  /// Informational state (blue)
  info,

  /// Success state (green)
  success,
}

/// Extension to get theme-aware colors for feedback types
extension FeedbackTypeColors on FeedbackType {
  /// Returns appropriate color for this feedback type based on theme
  Color getColor(BuildContext context) {
    final colorScheme = Theme.of(context).colorScheme;
    final brightness = Theme.of(context).brightness;

    switch (this) {
      case FeedbackType.warning:
        // Use a warm amber/orange tone that works in both light and dark modes
        return brightness == Brightness.light
            ? Colors.orange.shade700
            : Colors.orange.shade400;
      case FeedbackType.info:
        // Use theme's primary color for informational messages
        return colorScheme.primary;
      case FeedbackType.success:
        // Use a green tone that works in both light and dark modes
        return brightness == Brightness.light
            ? Colors.green.shade700
            : Colors.green.shade400;
    }
  }
}

class AlarmScreen extends ConsumerStatefulWidget {
  const AlarmScreen({super.key});

  @override
  ConsumerState<AlarmScreen> createState() => _AlarmScreenState();
}

class _AlarmScreenState extends ConsumerState<AlarmScreen> {
  AlarmMode _selectedMode = AlarmMode.standard;

  @override
  Widget build(BuildContext context) {
    final alarmService = ref.watch(alarmServiceProvider);
    final currentSensitivity = ref.watch(alarmSensitivityProvider);

    // Watch the alarm state stream for reactive updates
    final alarmStateAsync = ref.watch(alarmStateProvider);

    final viewModel = AlarmScreenViewModel(
      alarmState: alarmStateAsync.value ?? alarmService.currentState,
      sensitivity: currentSensitivity,
    );

    final displayConfig = viewModel.displayConfig;

    return Scaffold(
      appBar: AppBar(
        title: const Text('Guard Dog Alarm'),
        centerTitle: true,
      ),
      body: SafeArea(
        child: Padding(
          padding: const EdgeInsets.all(16.0),
          child: Column(
            crossAxisAlignment: CrossAxisAlignment.stretch,
            children: [
              // Alarm Status Card
              Expanded(
                child: _buildStatusCard(context, displayConfig),
              ),
              const SizedBox(height: 16),

              // Mode Selection (when inactive and not counting down)
              if (viewModel.shouldShowModeSelection) ...[
                _buildModeSelection(context),
                const SizedBox(height: 8),
              ],

              // Sensitivity Selection
              if (viewModel.shouldShowSensitivitySelection) ...[
                _buildSensitivitySelection(context, currentSensitivity),
                const SizedBox(height: 16),
              ],

              // Action Buttons
              _buildActionButton(
                context,
                displayConfig,
                alarmService,
              ),

              // Recalibrate button for active state
              if (displayConfig.state == AlarmDisplayState.active) ...[
                const SizedBox(height: 8),
                OutlinedButton(
                  onPressed: () => _handleAction(
                      context, AlarmAction.recalibrate, alarmService),
                  child: const Text('Recalibrate Sensors'),
                ),
              ],
            ],
          ),
        ),
      ),
    );
  }

  Widget _buildStatusCard(BuildContext context, AlarmDisplayConfig config) {
    return Card(
      color: config.backgroundColor,
      child: Padding(
        padding: const EdgeInsets.all(24.0),
        child: Column(
          mainAxisAlignment: MainAxisAlignment.center,
          children: [
            if (config.state == AlarmDisplayState.countdown)
              _buildCountdownDisplay(context, config)
            else
              _buildStatusDisplay(context, config),
          ],
        ),
      ),
    );
  }

  Widget _buildCountdownDisplay(
      BuildContext context, AlarmDisplayConfig config) {
    return Column(
      children: [
        Text(
          config.statusText,
          style: Theme.of(context).textTheme.titleMedium?.copyWith(
                color: config.foregroundColor,
                fontWeight: FontWeight.bold,
              ),
        ),
        const SizedBox(height: 16),
        Text(
          config.countdownText ?? '0',
          style: Theme.of(context).textTheme.displayLarge?.copyWith(
                color: config.foregroundColor,
                fontWeight: FontWeight.bold,
                fontSize: 80,
              ),
        ),
        const SizedBox(height: 16),
        Text(
          'seconds',
          style: Theme.of(context).textTheme.titleMedium?.copyWith(
                color: config.foregroundColor,
              ),
        ),
      ],
    );
  }

  Widget _buildStatusDisplay(BuildContext context, AlarmDisplayConfig config) {
    return Column(
      children: [
        Icon(
          config.icon,
          size: 100,
          color: config.foregroundColor,
        ),
        const SizedBox(height: 24),
        Text(
          config.statusText,
          style: Theme.of(context).textTheme.headlineSmall?.copyWith(
                fontWeight: FontWeight.bold,
                color: config.foregroundColor,
              ),
          textAlign: TextAlign.center,
        ),
        const SizedBox(height: 16),

        // Additional Info
        if (config.modeText != null) ...[
          Text(
            config.modeText!,
            style: Theme.of(context).textTheme.bodyLarge,
          ),
          const SizedBox(height: 8),
        ],
        if (config.sensitivityText != null) ...[
          Text(
            config.sensitivityText!,
            style: Theme.of(context).textTheme.bodyMedium,
          ),
          const SizedBox(height: 8),
        ],
        if (config.durationText != null) ...[
          Text(
            config.durationText!,
            style: Theme.of(context).textTheme.bodySmall?.copyWith(
                  color: Colors.grey.shade600,
                ),
          ),
        ],

        if (config.triggerCount != null) ...[
          const SizedBox(height: 16),
          Text(
            'Triggers: ${config.triggerCount}',
            style: Theme.of(context).textTheme.bodyMedium?.copyWith(
                  fontWeight: FontWeight.bold,
                ),
          ),
        ],
      ],
    );
  }

  Widget _buildModeSelection(BuildContext context) {
    return Column(
      crossAxisAlignment: CrossAxisAlignment.stretch,
      children: [
        Text(
          'Alarm Mode: ${_selectedMode.displayName}',
          style: Theme.of(context).textTheme.titleMedium,
        ),
        const SizedBox(height: 8),
        SegmentedButton<AlarmMode>(
          segments: AlarmMode.values
              .map((mode) => ButtonSegment<AlarmMode>(
                    value: mode,
                    label: Text(mode.displayName),
                    tooltip: mode.description,
                  ))
              .toList(),
          selected: {_selectedMode},
          onSelectionChanged: (Set<AlarmMode> selection) {
            setState(() {
              _selectedMode = selection.first;
            });
          },
        ),
      ],
    );
  }

  Widget _buildSensitivitySelection(
    BuildContext context,
    AlarmSensitivity currentSensitivity,
  ) {
    return Column(
      crossAxisAlignment: CrossAxisAlignment.stretch,
      children: [
        Text(
          'Sensitivity: ${currentSensitivity.name}',
          style: Theme.of(context).textTheme.titleMedium,
        ),
        const SizedBox(height: 8),
        SegmentedButton<AlarmSensitivity>(
          segments: const [
            ButtonSegment(
              value: AlarmSensitivity.low,
              label: Text('Low'),
            ),
            ButtonSegment(
              value: AlarmSensitivity.medium,
              label: Text('Med'),
            ),
            ButtonSegment(
              value: AlarmSensitivity.high,
              label: Text('High'),
            ),
            ButtonSegment(
              value: AlarmSensitivity.veryHigh,
              label: Text('Max'),
            ),
          ],
          selected: {currentSensitivity},
          onSelectionChanged: (Set<AlarmSensitivity> selection) {
            final service = ref.read(appSettingsServiceProvider);
            final sensitivityName = service.getSensitivityName(selection.first);
            ref
                .read(appSettingsProvider.notifier)
                .setSensitivityLevel(sensitivityName);
          },
        ),
      ],
    );
  }

  Widget _buildActionButton(
    BuildContext context,
    AlarmDisplayConfig config,
    AlarmService alarmService,
  ) {
    final buttonConfig = config.buttonConfig;

    return ElevatedButton(
      onPressed: () =>
          _handleAction(context, buttonConfig.action, alarmService),
      style: ElevatedButton.styleFrom(
        padding: const EdgeInsets.symmetric(vertical: 20),
        backgroundColor: buttonConfig.backgroundColor,
        foregroundColor: buttonConfig.foregroundColor,
      ),
      child: Row(
        mainAxisAlignment: MainAxisAlignment.center,
        children: [
          Icon(buttonConfig.icon, size: 28),
          const SizedBox(width: 12),
          Text(
            buttonConfig.text,
            style: const TextStyle(
              fontSize: 18,
              fontWeight: FontWeight.bold,
            ),
          ),
        ],
      ),
    );
  }

  /// Shows a snackbar with feedback to the user
  void _showFeedback(BuildContext context, String message, FeedbackType type) {
    if (!context.mounted) return;

    ScaffoldMessenger.of(context).showSnackBar(
      SnackBar(
        content: Text(message),
        backgroundColor: type.getColor(context),
        duration: const Duration(seconds: 2),
      ),
    );
  }

  Future<void> _handleAction(
    BuildContext context,
    AlarmAction action,
    AlarmService alarmService,
  ) async {
    switch (action) {
      case AlarmAction.activate:
        await alarmService.startActivation(mode: _selectedMode);
        if (context.mounted) {
          _showFeedback(
              context, AlarmFeedbackMessages.activating, FeedbackType.warning);
        }
        break;
      case AlarmAction.deactivate:
        await _handleDeactivate(context, alarmService);
        break;
      case AlarmAction.cancelCountdown:
        await alarmService.cancelCountdown();
        if (context.mounted) {
          _showFeedback(
              context, AlarmFeedbackMessages.cancelled, FeedbackType.warning);
        }
        break;
      case AlarmAction.acknowledge:
        await alarmService.acknowledge();
        if (context.mounted) {
          _showFeedback(
              context, AlarmFeedbackMessages.acknowledged, FeedbackType.info);
        }
        break;
      case AlarmAction.recalibrate:
        await alarmService.recalibrate();
        if (context.mounted) {
          _showFeedback(context, AlarmFeedbackMessages.recalibrated,
              FeedbackType.success);
        }
        break;
    }
  }

  Future<void> _handleDeactivate(
      BuildContext context, AlarmService alarmService) async {
    // Show unlock dialog
    await showDialog(
      context: context,
      barrierDismissible: false,
      builder: (context) => UnlockDialog(
        onUnlockAttempt: (code) async {
          final success = await alarmService.deactivateWithUnlockCode(code);
          if (context.mounted) {
            if (success) {
              Navigator.of(context).pop();
              ScaffoldMessenger.of(context).showSnackBar(
                const SnackBar(
                  content: Text('Alarm deactivated successfully'),
                  backgroundColor: Colors.green,
                ),
              );
            } else {
              ScaffoldMessenger.of(context).showSnackBar(
                const SnackBar(
                  content: Text('Invalid unlock code'),
                  backgroundColor: Colors.red,
                ),
              );
            }
          }
        },
      ),
    );
  }
}
