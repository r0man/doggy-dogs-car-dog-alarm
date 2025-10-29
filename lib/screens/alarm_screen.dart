import 'package:flutter/material.dart' hide TimeOfDay;
import 'package:flutter_riverpod/flutter_riverpod.dart';
import '../models/alarm_state.dart';
import '../models/sensor_data.dart';
import '../models/neighborhood.dart';
import '../services/alarm_service.dart';
import '../services/sensor_detection_service.dart';
import '../services/app_settings_service.dart';
import '../providers/achievement_provider.dart';
import '../widgets/unlock_dialog.dart';

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
                child: Card(
                  color: alarmService.currentState.isTriggered
                      ? Colors.red.shade50
                      : alarmService.currentState.isCountingDown
                          ? Colors.orange.shade50
                          : alarmService.currentState.isActive
                              ? Colors.green.shade50
                              : Colors.grey.shade50,
                  child: Padding(
                    padding: const EdgeInsets.all(24.0),
                    child: Column(
                      mainAxisAlignment: MainAxisAlignment.center,
                      children: [
                        // Countdown Display
                        if (alarmService.currentState.isCountingDown) ...[
                          Text(
                            'ACTIVATING IN',
                            style: Theme.of(context)
                                .textTheme
                                .titleMedium
                                ?.copyWith(
                                  color: Colors.orange,
                                  fontWeight: FontWeight.bold,
                                ),
                          ),
                          const SizedBox(height: 16),
                          Text(
                            '${alarmService.currentState.countdownSeconds}',
                            style: Theme.of(context)
                                .textTheme
                                .displayLarge
                                ?.copyWith(
                                  color: Colors.orange,
                                  fontWeight: FontWeight.bold,
                                  fontSize: 80,
                                ),
                          ),
                          const SizedBox(height: 16),
                          Text(
                            'seconds',
                            style: Theme.of(context)
                                .textTheme
                                .titleMedium
                                ?.copyWith(
                                  color: Colors.orange.shade700,
                                ),
                          ),
                        ] else ...[
                          // Status Icon
                          Icon(
                            alarmService.currentState.isTriggered
                                ? Icons.warning_amber_rounded
                                : alarmService.currentState.isActive
                                    ? Icons.security
                                    : Icons.security_outlined,
                            size: 100,
                            color: alarmService.currentState.isTriggered
                                ? Colors.red
                                : alarmService.currentState.isActive
                                    ? Colors.green
                                    : Colors.grey,
                          ),
                          const SizedBox(height: 24),

                          // Status Text
                          Text(
                            alarmService.currentState.isTriggered
                                ? 'ALARM TRIGGERED!'
                                : alarmService.currentState.isActive
                                    ? 'GUARD DOG ACTIVE'
                                    : 'GUARD DOG SLEEPING',
                            style: Theme.of(context)
                                .textTheme
                                .headlineSmall
                                ?.copyWith(
                                  fontWeight: FontWeight.bold,
                                  color: alarmService.currentState.isTriggered
                                      ? Colors.red
                                      : alarmService.currentState.isActive
                                          ? Colors.green
                                          : Colors.grey,
                                ),
                            textAlign: TextAlign.center,
                          ),
                          const SizedBox(height: 16),

                          // Additional Info
                          if (alarmService.currentState.isActive) ...[
                            Text(
                              'Mode: ${alarmService.currentState.mode.displayName}',
                              style: Theme.of(context).textTheme.bodyLarge,
                            ),
                            const SizedBox(height: 8),
                            Text(
                              'Sensitivity: ${currentSensitivity.name}',
                              style: Theme.of(context).textTheme.bodyMedium,
                            ),
                            if (alarmService.currentState.activeDuration !=
                                null) ...[
                              const SizedBox(height: 8),
                              Text(
                                'Active for: ${_formatDuration(alarmService.currentState.activeDuration!)}',
                                style: Theme.of(context)
                                    .textTheme
                                    .bodySmall
                                    ?.copyWith(
                                      color: Colors.grey.shade600,
                                    ),
                              ),
                            ],
                          ],

                          if (alarmService.currentState.triggerCount > 0) ...[
                            const SizedBox(height: 16),
                            Text(
                              'Triggers: ${alarmService.currentState.triggerCount}',
                              style: Theme.of(context)
                                  .textTheme
                                  .bodyMedium
                                  ?.copyWith(
                                    fontWeight: FontWeight.bold,
                                  ),
                            ),
                          ],
                        ],
                      ],
                    ),
                  ),
                ),
              ),
              const SizedBox(height: 16),

              // Mode Selection (when inactive and not counting down)
              if (!alarmService.currentState.isActive &&
                  !alarmService.currentState.isCountingDown) ...[
                Text(
                  'Alarm Mode',
                  style: Theme.of(context).textTheme.titleMedium,
                ),
                const SizedBox(height: 8),
                ...AlarmMode.values.map((mode) => RadioListTile<AlarmMode>(
                      title: Text(mode.displayName),
                      subtitle: Text(mode.description),
                      value: mode,
                      groupValue: _selectedMode,
                      onChanged: (value) {
                        if (value != null) {
                          setState(() {
                            _selectedMode = value;
                          });
                        }
                      },
                    )),
                const SizedBox(height: 8),

                // Sensitivity Selection
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
                    final sensitivityName =
                        service.getSensitivityName(selection.first);
                    ref
                        .read(appSettingsProvider.notifier)
                        .setSensitivityLevel(sensitivityName);
                  },
                ),
                const SizedBox(height: 16),
              ],

              // Action Buttons
              if (alarmService.currentState.isCountingDown)
                // Cancel button during countdown
                ElevatedButton(
                  onPressed: () => alarmService.cancelCountdown(),
                  style: ElevatedButton.styleFrom(
                    padding: const EdgeInsets.symmetric(vertical: 20),
                    backgroundColor: Colors.orange,
                    foregroundColor: Colors.white,
                  ),
                  child: const Row(
                    mainAxisAlignment: MainAxisAlignment.center,
                    children: [
                      Icon(Icons.cancel, size: 28),
                      SizedBox(width: 12),
                      Text(
                        'CANCEL ACTIVATION',
                        style: TextStyle(
                          fontSize: 18,
                          fontWeight: FontWeight.bold,
                        ),
                      ),
                    ],
                  ),
                )
              else if (alarmService.currentState.isTriggered)
                ElevatedButton(
                  onPressed: () => alarmService.acknowledge(),
                  style: ElevatedButton.styleFrom(
                    padding: const EdgeInsets.symmetric(vertical: 20),
                    backgroundColor: Colors.orange,
                    foregroundColor: Colors.white,
                  ),
                  child: const Row(
                    mainAxisAlignment: MainAxisAlignment.center,
                    children: [
                      Icon(Icons.check_circle, size: 28),
                      SizedBox(width: 12),
                      Text(
                        'ACKNOWLEDGE',
                        style: TextStyle(
                          fontSize: 18,
                          fontWeight: FontWeight.bold,
                        ),
                      ),
                    ],
                  ),
                )
              else if (alarmService.currentState.isActive) ...[
                ElevatedButton(
                  onPressed: () => _handleDeactivate(context, alarmService),
                  style: ElevatedButton.styleFrom(
                    padding: const EdgeInsets.symmetric(vertical: 20),
                    backgroundColor: Colors.red,
                    foregroundColor: Colors.white,
                  ),
                  child: const Row(
                    mainAxisAlignment: MainAxisAlignment.center,
                    children: [
                      Icon(Icons.stop, size: 28),
                      SizedBox(width: 12),
                      Text(
                        'DEACTIVATE ALARM',
                        style: TextStyle(
                          fontSize: 18,
                          fontWeight: FontWeight.bold,
                        ),
                      ),
                    ],
                  ),
                ),
                const SizedBox(height: 8),
                OutlinedButton(
                  onPressed: () => alarmService.recalibrate(),
                  child: const Text('Recalibrate Sensors'),
                ),
              ] else
                ElevatedButton(
                  onPressed: () async {
                    // Track alarm activation achievements
                    await incrementAchievement(ref, 'first_timer');
                    await incrementAchievement(ref, 'bark_and_disorderly');

                    // Track night shift achievement if it's late night
                    final timeOfDay =
                        TimeOfDayExtension.fromDateTime(DateTime.now());
                    if (timeOfDay == TimeOfDay.lateNight) {
                      await incrementAchievement(ref, 'night_shift');
                    }

                    alarmService.startActivation(mode: _selectedMode);
                  },
                  style: ElevatedButton.styleFrom(
                    padding: const EdgeInsets.symmetric(vertical: 20),
                    backgroundColor: Colors.green,
                    foregroundColor: Colors.white,
                  ),
                  child: const Row(
                    mainAxisAlignment: MainAxisAlignment.center,
                    children: [
                      Icon(Icons.security, size: 28),
                      SizedBox(width: 12),
                      Text(
                        'ACTIVATE GUARD DOG',
                        style: TextStyle(
                          fontSize: 18,
                          fontWeight: FontWeight.bold,
                        ),
                      ),
                    ],
                  ),
                ),
            ],
          ),
        ),
      ),
    );
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

  String _formatDuration(Duration duration) {
    if (duration.inHours > 0) {
      return '${duration.inHours}h ${duration.inMinutes.remainder(60)}m';
    } else if (duration.inMinutes > 0) {
      return '${duration.inMinutes}m ${duration.inSeconds.remainder(60)}s';
    } else {
      return '${duration.inSeconds}s';
    }
  }
}
