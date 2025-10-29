import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:package_info_plus/package_info_plus.dart';
import '../models/app_settings.dart';
import '../services/app_settings_service.dart';
import '../services/unlock_code_service.dart';

class SettingsScreen extends ConsumerStatefulWidget {
  const SettingsScreen({super.key});

  @override
  ConsumerState<SettingsScreen> createState() => _SettingsScreenState();
}

class _SettingsScreenState extends ConsumerState<SettingsScreen> {
  @override
  Widget build(BuildContext context) {
    final settings = ref.watch(appSettingsProvider);

    return Scaffold(
      appBar: AppBar(
        title: const Text('Settings'),
        centerTitle: true,
      ),
      body: ListView(
        padding: const EdgeInsets.all(16.0),
        children: [
          _buildAlarmSection(settings),
          const SizedBox(height: 24),
          _buildNotificationsSection(settings),
          const SizedBox(height: 24),
          _buildBatterySection(settings),
          const SizedBox(height: 24),
          _buildSecuritySection(),
          const SizedBox(height: 24),
          _buildAboutSection(),
          const SizedBox(height: 24),
          _buildResetSection(),
        ],
      ),
    );
  }

  Widget _buildAlarmSection(AppSettings settings) {
    return Card(
      child: Padding(
        padding: const EdgeInsets.all(16.0),
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            Row(
              children: [
                const Icon(Icons.security, color: Colors.orange),
                const SizedBox(width: 8),
                Text(
                  'Alarm Settings',
                  style: Theme.of(context).textTheme.titleLarge,
                ),
              ],
            ),
            const SizedBox(height: 16),

            // Countdown Duration
            Text(
              'Activation Countdown',
              style: Theme.of(context).textTheme.titleMedium,
            ),
            const SizedBox(height: 8),
            Row(
              children: [
                Expanded(
                  child: Slider(
                    value: settings.countdownDuration.toDouble(),
                    min: 15,
                    max: 120,
                    divisions: 21, // 5-second increments
                    label: '${settings.countdownDuration}s',
                    onChanged: (value) {
                      ref.read(appSettingsProvider.notifier)
                          .setCountdownDuration(value.toInt());
                    },
                  ),
                ),
                SizedBox(
                  width: 60,
                  child: Text(
                    '${settings.countdownDuration}s',
                    style: Theme.of(context).textTheme.titleMedium,
                    textAlign: TextAlign.right,
                  ),
                ),
              ],
            ),
            Text(
              'Time before alarm activates after pressing start',
              style: Theme.of(context).textTheme.bodySmall?.copyWith(
                    color: Colors.grey.shade600,
                  ),
            ),
            const SizedBox(height: 16),

            // Sensitivity
            Text(
              'Sensor Sensitivity',
              style: Theme.of(context).textTheme.titleMedium,
            ),
            const SizedBox(height: 8),
            SegmentedButton<String>(
              segments: const [
                ButtonSegment(value: 'low', label: Text('Low')),
                ButtonSegment(value: 'medium', label: Text('Med')),
                ButtonSegment(value: 'high', label: Text('High')),
                ButtonSegment(value: 'veryHigh', label: Text('Max')),
              ],
              selected: {settings.sensitivityLevel},
              onSelectionChanged: (Set<String> selection) {
                ref.read(appSettingsProvider.notifier)
                    .setSensitivityLevel(selection.first);
              },
            ),
            const SizedBox(height: 8),
            Text(
              _getSensitivityDescription(settings.sensitivityLevel),
              style: Theme.of(context).textTheme.bodySmall?.copyWith(
                    color: Colors.grey.shade600,
                  ),
            ),
            const SizedBox(height: 16),

            // Bark Volume
            Text(
              'Bark Volume',
              style: Theme.of(context).textTheme.titleMedium,
            ),
            const SizedBox(height: 8),
            Row(
              children: [
                const Icon(Icons.volume_down, size: 20),
                Expanded(
                  child: Slider(
                    value: settings.barkVolume,
                    min: 0.0,
                    max: 1.0,
                    divisions: 20,
                    label: '${(settings.barkVolume * 100).toInt()}%',
                    onChanged: (value) {
                      ref.read(appSettingsProvider.notifier)
                          .setBarkVolume(value);
                    },
                  ),
                ),
                const Icon(Icons.volume_up, size: 20),
                SizedBox(
                  width: 50,
                  child: Text(
                    '${(settings.barkVolume * 100).toInt()}%',
                    style: Theme.of(context).textTheme.titleMedium,
                    textAlign: TextAlign.right,
                  ),
                ),
              ],
            ),
          ],
        ),
      ),
    );
  }

  Widget _buildNotificationsSection(AppSettings settings) {
    return Card(
      child: Padding(
        padding: const EdgeInsets.all(16.0),
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            Row(
              children: [
                const Icon(Icons.notifications, color: Colors.blue),
                const SizedBox(width: 8),
                Text(
                  'Notifications',
                  style: Theme.of(context).textTheme.titleLarge,
                ),
              ],
            ),
            const SizedBox(height: 16),
            SwitchListTile(
              title: const Text('Enable Notifications'),
              subtitle: const Text('Receive alerts when alarm is triggered'),
              value: settings.notificationsEnabled,
              onChanged: (value) {
                ref.read(appSettingsProvider.notifier)
                    .setNotificationsEnabled(value);
              },
            ),
          ],
        ),
      ),
    );
  }

  Widget _buildBatterySection(AppSettings settings) {
    return Card(
      child: Padding(
        padding: const EdgeInsets.all(16.0),
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            Row(
              children: [
                const Icon(Icons.battery_charging_full, color: Colors.green),
                const SizedBox(width: 8),
                Text(
                  'Battery',
                  style: Theme.of(context).textTheme.titleLarge,
                ),
              ],
            ),
            const SizedBox(height: 16),
            SwitchListTile(
              title: const Text('Battery Optimization'),
              subtitle: const Text(
                'Reduce power usage when alarm is inactive',
              ),
              value: settings.batteryOptimizationEnabled,
              onChanged: (value) {
                ref.read(appSettingsProvider.notifier)
                    .setBatteryOptimizationEnabled(value);
              },
            ),
          ],
        ),
      ),
    );
  }

  Widget _buildSecuritySection() {
    return Card(
      child: Padding(
        padding: const EdgeInsets.all(16.0),
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            Row(
              children: [
                const Icon(Icons.lock, color: Colors.red),
                const SizedBox(width: 8),
                Text(
                  'Security',
                  style: Theme.of(context).textTheme.titleLarge,
                ),
              ],
            ),
            const SizedBox(height: 16),
            ListTile(
              leading: const Icon(Icons.pin),
              title: const Text('Change Unlock Code'),
              subtitle: const Text('Set a new PIN to deactivate the alarm'),
              trailing: const Icon(Icons.chevron_right),
              onTap: () => _showChangeUnlockCodeDialog(),
            ),
          ],
        ),
      ),
    );
  }

  Widget _buildAboutSection() {
    return Card(
      child: Padding(
        padding: const EdgeInsets.all(16.0),
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            Row(
              children: [
                const Icon(Icons.info, color: Colors.grey),
                const SizedBox(width: 8),
                Text(
                  'About',
                  style: Theme.of(context).textTheme.titleLarge,
                ),
              ],
            ),
            const SizedBox(height: 16),
            FutureBuilder<PackageInfo>(
              future: PackageInfo.fromPlatform(),
              builder: (context, snapshot) {
                if (!snapshot.hasData) {
                  return const CircularProgressIndicator();
                }

                final info = snapshot.data!;
                return Column(
                  children: [
                    ListTile(
                      leading: const Icon(Icons.pets),
                      title: const Text('Doggy Dogs Car Dog Alarm'),
                      subtitle: Text('Version ${info.version} (${info.buildNumber})'),
                    ),
                    ListTile(
                      leading: const Icon(Icons.code),
                      title: const Text('App ID'),
                      subtitle: Text(info.packageName),
                    ),
                  ],
                );
              },
            ),
          ],
        ),
      ),
    );
  }

  Widget _buildResetSection() {
    return Card(
      child: Padding(
        padding: const EdgeInsets.all(16.0),
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            Row(
              children: [
                const Icon(Icons.restore, color: Colors.orange),
                const SizedBox(width: 8),
                Text(
                  'Reset',
                  style: Theme.of(context).textTheme.titleLarge,
                ),
              ],
            ),
            const SizedBox(height: 16),
            ListTile(
              leading: const Icon(Icons.settings_backup_restore),
              title: const Text('Reset to Defaults'),
              subtitle: const Text('Restore all settings to default values'),
              trailing: const Icon(Icons.chevron_right),
              onTap: () => _showResetConfirmation(),
            ),
          ],
        ),
      ),
    );
  }

  String _getSensitivityDescription(String level) {
    switch (level) {
      case 'low':
        return 'Triggers only on significant movement';
      case 'high':
        return 'Triggers on light movement';
      case 'veryHigh':
        return 'Most sensitive - triggers easily';
      case 'medium':
      default:
        return 'Balanced sensitivity for normal use';
    }
  }

  Future<void> _showChangeUnlockCodeDialog() async {
    final unlockService = ref.read(unlockCodeServiceProvider);

    final oldCodeController = TextEditingController();
    final newCodeController = TextEditingController();
    final confirmCodeController = TextEditingController();

    await showDialog(
      context: context,
      builder: (context) => AlertDialog(
        title: const Text('Change Unlock Code'),
        content: Column(
          mainAxisSize: MainAxisSize.min,
          children: [
            TextField(
              controller: oldCodeController,
              decoration: const InputDecoration(
                labelText: 'Current Code',
                hintText: 'Enter current PIN',
              ),
              obscureText: true,
              keyboardType: TextInputType.number,
              maxLength: 6,
            ),
            const SizedBox(height: 16),
            TextField(
              controller: newCodeController,
              decoration: const InputDecoration(
                labelText: 'New Code',
                hintText: 'Enter new PIN',
              ),
              obscureText: true,
              keyboardType: TextInputType.number,
              maxLength: 6,
            ),
            const SizedBox(height: 16),
            TextField(
              controller: confirmCodeController,
              decoration: const InputDecoration(
                labelText: 'Confirm New Code',
                hintText: 'Re-enter new PIN',
              ),
              obscureText: true,
              keyboardType: TextInputType.number,
              maxLength: 6,
            ),
          ],
        ),
        actions: [
          TextButton(
            onPressed: () => Navigator.of(context).pop(),
            child: const Text('CANCEL'),
          ),
          ElevatedButton(
            onPressed: () async {
              // Validate old code
              final oldCode = oldCodeController.text.trim();
              final isValid = await unlockService.validateUnlockCode(oldCode);

              if (!isValid) {
                if (context.mounted) {
                  ScaffoldMessenger.of(context).showSnackBar(
                    const SnackBar(
                      content: Text('Current code is incorrect'),
                      backgroundColor: Colors.red,
                    ),
                  );
                }
                return;
              }

              // Validate new codes match
              final newCode = newCodeController.text.trim();
              final confirmCode = confirmCodeController.text.trim();

              if (newCode != confirmCode) {
                if (context.mounted) {
                  ScaffoldMessenger.of(context).showSnackBar(
                    const SnackBar(
                      content: Text('New codes do not match'),
                      backgroundColor: Colors.red,
                    ),
                  );
                }
                return;
              }

              if (newCode.length < 4) {
                if (context.mounted) {
                  ScaffoldMessenger.of(context).showSnackBar(
                    const SnackBar(
                      content: Text('Code must be at least 4 digits'),
                      backgroundColor: Colors.red,
                    ),
                  );
                }
                return;
              }

              // Set new code
              await unlockService.setUnlockCode(newCode);

              if (context.mounted) {
                Navigator.of(context).pop();
                ScaffoldMessenger.of(context).showSnackBar(
                  const SnackBar(
                    content: Text('Unlock code changed successfully'),
                    backgroundColor: Colors.green,
                  ),
                );
              }
            },
            child: const Text('CHANGE CODE'),
          ),
        ],
      ),
    );
  }

  Future<void> _showResetConfirmation() async {
    final confirmed = await showDialog<bool>(
      context: context,
      builder: (context) => AlertDialog(
        title: const Text('Reset Settings?'),
        content: const Text(
          'This will restore all settings to their default values. This action cannot be undone.',
        ),
        actions: [
          TextButton(
            onPressed: () => Navigator.of(context).pop(false),
            child: const Text('CANCEL'),
          ),
          ElevatedButton(
            onPressed: () => Navigator.of(context).pop(true),
            style: ElevatedButton.styleFrom(backgroundColor: Colors.red),
            child: const Text('RESET'),
          ),
        ],
      ),
    );

    if (confirmed == true && mounted) {
      await ref.read(appSettingsProvider.notifier).resetToDefaults();
      if (mounted) {
        ScaffoldMessenger.of(context).showSnackBar(
          const SnackBar(
            content: Text('Settings reset to defaults'),
            backgroundColor: Colors.green,
          ),
        );
      }
    }
  }
}
