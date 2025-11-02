import 'dart:convert';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:shared_preferences/shared_preferences.dart';
import '../models/app_settings.dart';
import '../models/sensor_data.dart';
import 'unlock_code_service.dart';

/// Service for managing app settings with persistence
class AppSettingsService {
  static const String _settingsKey = 'app_settings';

  final SharedPreferences _prefs;

  AppSettingsService(this._prefs);

  /// Load settings from storage
  Future<AppSettings> loadSettings() async {
    final jsonString = _prefs.getString(_settingsKey);

    if (jsonString == null) {
      return const AppSettings(); // Return defaults
    }

    try {
      final json = jsonDecode(jsonString) as Map<String, dynamic>;
      return AppSettings.fromJson(json);
    } catch (e) {
      // If parsing fails, return defaults
      return const AppSettings();
    }
  }

  /// Save settings to storage
  Future<void> saveSettings(AppSettings settings) async {
    if (!settings.isValid()) {
      throw ArgumentError('Invalid settings cannot be saved');
    }

    final jsonString = jsonEncode(settings.toJson());
    await _prefs.setString(_settingsKey, jsonString);
  }

  /// Clear all settings (reset to defaults)
  Future<void> clearSettings() async {
    await _prefs.remove(_settingsKey);
  }

  /// Get alarm sensitivity from settings
  AlarmSensitivity getAlarmSensitivity(AppSettings settings) {
    switch (settings.sensitivityLevel) {
      case 'low':
        return AlarmSensitivity.low;
      case 'high':
        return AlarmSensitivity.high;
      case 'veryHigh':
        return AlarmSensitivity.veryHigh;
      case 'medium':
      default:
        return AlarmSensitivity.medium;
    }
  }

  /// Get sensitivity name from AlarmSensitivity
  String getSensitivityName(AlarmSensitivity sensitivity) {
    if (sensitivity == AlarmSensitivity.low) return 'low';
    if (sensitivity == AlarmSensitivity.high) return 'high';
    if (sensitivity == AlarmSensitivity.veryHigh) return 'veryHigh';
    return 'medium';
  }
}

/// Provider for AppSettingsService
final appSettingsServiceProvider = Provider<AppSettingsService>((ref) {
  final prefs = ref.watch(sharedPreferencesProvider);
  return AppSettingsService(prefs);
});

/// StateNotifier for managing app settings
class AppSettingsNotifier extends StateNotifier<AppSettings> {
  final AppSettingsService _service;

  AppSettingsNotifier(this._service) : super(const AppSettings()) {
    _loadSettings();
  }

  /// Load settings from storage
  Future<void> _loadSettings() async {
    final settings = await _service.loadSettings();
    state = settings;
  }

  /// Update countdown duration
  Future<void> setCountdownDuration(int duration) async {
    if (duration < 10 || duration > 120) {
      throw ArgumentError(
          'Countdown duration must be between 10 and 120 seconds');
    }

    final newSettings = state.copyWith(countdownDuration: duration);
    await _service.saveSettings(newSettings);
    state = newSettings;
  }

  /// Update sensitivity level
  Future<void> setSensitivityLevel(String level) async {
    if (!['low', 'medium', 'high', 'veryHigh'].contains(level)) {
      throw ArgumentError('Invalid sensitivity level');
    }

    final newSettings = state.copyWith(sensitivityLevel: level);
    await _service.saveSettings(newSettings);
    state = newSettings;
  }

  /// Update bark volume
  Future<void> setBarkVolume(double volume) async {
    if (volume < 0.0 || volume > 1.0) {
      throw ArgumentError('Bark volume must be between 0.0 and 1.0');
    }

    final newSettings = state.copyWith(barkVolume: volume);
    await _service.saveSettings(newSettings);
    state = newSettings;
  }

  /// Toggle notifications
  Future<void> setNotificationsEnabled(bool enabled) async {
    final newSettings = state.copyWith(notificationsEnabled: enabled);
    await _service.saveSettings(newSettings);
    state = newSettings;
  }

  /// Toggle battery optimization
  Future<void> setBatteryOptimizationEnabled(bool enabled) async {
    final newSettings = state.copyWith(batteryOptimizationEnabled: enabled);
    await _service.saveSettings(newSettings);
    state = newSettings;
  }

  /// Reset to defaults
  Future<void> resetToDefaults() async {
    await _service.clearSettings();
    state = const AppSettings();
  }
}

/// Provider for app settings state
final appSettingsProvider =
    StateNotifierProvider<AppSettingsNotifier, AppSettings>((ref) {
  final service = ref.watch(appSettingsServiceProvider);
  return AppSettingsNotifier(service);
});
