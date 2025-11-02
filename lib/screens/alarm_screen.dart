import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import '../models/alarm_state.dart';
import '../models/sensor_data.dart';
import '../services/alarm_service.dart';
import '../services/sensor_detection_service.dart';
import '../services/app_settings_service.dart';
import '../widgets/unlock_dialog.dart';
import 'alarm_screen_view_model.dart';

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

  Future<void> _handleAction(
    BuildContext context,
    AlarmAction action,
    AlarmService alarmService,
  ) async {
    switch (action) {
      case AlarmAction.activate:
        await alarmService.startActivation(mode: _selectedMode);
        if (context.mounted) {
          ScaffoldMessenger.of(context).showSnackBar(
            const SnackBar(
              content: Text('Activating Guard Dog...'),
              backgroundColor: Colors.orange,
              duration: Duration(seconds: 2),
            ),
          );
        }
        break;
      case AlarmAction.deactivate:
        await _handleDeactivate(context, alarmService);
        break;
      case AlarmAction.cancelCountdown:
        // Show feedback before async operation to avoid context issues
        if (context.mounted) {
          ScaffoldMessenger.of(context).showSnackBar(
            const SnackBar(
              content: Text('Activation cancelled'),
              backgroundColor: Colors.orange,
              duration: Duration(seconds: 2),
            ),
          );
        }
        await alarmService.cancelCountdown();
        break;
      case AlarmAction.acknowledge:
        await alarmService.acknowledge();
        if (context.mounted) {
          ScaffoldMessenger.of(context).showSnackBar(
            const SnackBar(
              content: Text('Alarm acknowledged'),
              backgroundColor: Colors.blue,
              duration: Duration(seconds: 2),
            ),
          );
        }
        break;
      case AlarmAction.recalibrate:
        await alarmService.recalibrate();
        if (context.mounted) {
          ScaffoldMessenger.of(context).showSnackBar(
            const SnackBar(
              content: Text('Sensors recalibrated'),
              backgroundColor: Colors.green,
              duration: Duration(seconds: 2),
            ),
          );
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
