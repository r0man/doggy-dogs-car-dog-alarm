import 'package:flutter/material.dart';
import 'package:shared_preferences/shared_preferences.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import '../models/alarm_state.dart';

/// Keys for shared preferences
class PersistenceKeys {
  static const alarmIsActive = 'alarm_is_active';
  static const alarmIsTriggered = 'alarm_is_triggered';
  static const alarmIsCountingDown = 'alarm_is_counting_down';
  static const alarmCountdownSeconds = 'alarm_countdown_seconds';
  static const alarmMode = 'alarm_mode';
  static const alarmActivatedAt = 'alarm_activated_at';
  static const alarmLastTriggeredAt = 'alarm_last_triggered_at';
  static const alarmTriggerCount = 'alarm_trigger_count';
  static const dogId = 'current_dog_id';
  static const lastKnownSensitivity = 'last_known_sensitivity';
}

/// Service for persisting alarm state across app restarts
class AlarmPersistenceService {
  static final AlarmPersistenceService _instance = AlarmPersistenceService._internal();

  factory AlarmPersistenceService() => _instance;

  AlarmPersistenceService._internal();

  SharedPreferences? _prefs;

  /// Initialize the service
  Future<void> initialize() async {
    if (_prefs != null) return;

    debugPrint('💾 Initializing alarm persistence service...');

    try {
      _prefs = await SharedPreferences.getInstance();
      debugPrint('✅ Alarm persistence service initialized');
    } catch (e) {
      debugPrint('❌ Failed to initialize persistence: $e');
      rethrow;
    }
  }

  /// Save alarm state
  Future<void> saveAlarmState(AlarmState state) async {
    await _ensureInitialized();

    debugPrint('💾 Saving alarm state: active=${state.isActive}, triggered=${state.isTriggered}');

    try {
      await _prefs!.setBool(PersistenceKeys.alarmIsActive, state.isActive);
      await _prefs!.setBool(PersistenceKeys.alarmIsTriggered, state.isTriggered);
      await _prefs!.setBool(PersistenceKeys.alarmIsCountingDown, state.isCountingDown);
      await _prefs!.setInt(PersistenceKeys.alarmCountdownSeconds, state.countdownSeconds);
      await _prefs!.setString(PersistenceKeys.alarmMode, state.mode.name);
      await _prefs!.setInt(PersistenceKeys.alarmTriggerCount, state.triggerCount);

      if (state.activatedAt != null) {
        await _prefs!.setString(
          PersistenceKeys.alarmActivatedAt,
          state.activatedAt!.toIso8601String(),
        );
      } else {
        await _prefs!.remove(PersistenceKeys.alarmActivatedAt);
      }

      if (state.lastTriggeredAt != null) {
        await _prefs!.setString(
          PersistenceKeys.alarmLastTriggeredAt,
          state.lastTriggeredAt!.toIso8601String(),
        );
      } else {
        await _prefs!.remove(PersistenceKeys.alarmLastTriggeredAt);
      }

      debugPrint('✅ Alarm state saved');
    } catch (e) {
      debugPrint('❌ Failed to save alarm state: $e');
    }
  }

  /// Load alarm state
  Future<AlarmState?> loadAlarmState() async {
    await _ensureInitialized();

    debugPrint('📖 Loading alarm state...');

    try {
      final isActive = _prefs!.getBool(PersistenceKeys.alarmIsActive);
      if (isActive == null || !isActive) {
        debugPrint('ℹ️  No active alarm state found');
        return null;
      }

      final isTriggered = _prefs!.getBool(PersistenceKeys.alarmIsTriggered) ?? false;
      final isCountingDown = _prefs!.getBool(PersistenceKeys.alarmIsCountingDown) ?? false;
      final countdownSeconds = _prefs!.getInt(PersistenceKeys.alarmCountdownSeconds) ?? 0;
      final triggerCount = _prefs!.getInt(PersistenceKeys.alarmTriggerCount) ?? 0;
      final modeStr = _prefs!.getString(PersistenceKeys.alarmMode);
      final activatedAtStr = _prefs!.getString(PersistenceKeys.alarmActivatedAt);
      final lastTriggeredAtStr = _prefs!.getString(PersistenceKeys.alarmLastTriggeredAt);

      final mode = AlarmMode.values.firstWhere(
        (m) => m.name == modeStr,
        orElse: () => AlarmMode.standard,
      );

      final activatedAt = activatedAtStr != null ? DateTime.tryParse(activatedAtStr) : null;

      final lastTriggeredAt =
          lastTriggeredAtStr != null ? DateTime.tryParse(lastTriggeredAtStr) : null;

      final state = AlarmState(
        isActive: isActive,
        isTriggered: isTriggered,
        isCountingDown: isCountingDown,
        countdownSeconds: countdownSeconds,
        mode: mode,
        activatedAt: activatedAt,
        lastTriggeredAt: lastTriggeredAt,
        triggerCount: triggerCount,
      );

      debugPrint('✅ Alarm state loaded: active=$isActive, triggered=$isTriggered');
      return state;
    } catch (e) {
      debugPrint('❌ Failed to load alarm state: $e');
      return null;
    }
  }

  /// Clear alarm state (when alarm is deactivated)
  Future<void> clearAlarmState() async {
    await _ensureInitialized();

    debugPrint('🗑️  Clearing alarm state...');

    try {
      await _prefs!.remove(PersistenceKeys.alarmIsActive);
      await _prefs!.remove(PersistenceKeys.alarmIsTriggered);
      await _prefs!.remove(PersistenceKeys.alarmIsCountingDown);
      await _prefs!.remove(PersistenceKeys.alarmCountdownSeconds);
      await _prefs!.remove(PersistenceKeys.alarmMode);
      await _prefs!.remove(PersistenceKeys.alarmActivatedAt);
      await _prefs!.remove(PersistenceKeys.alarmLastTriggeredAt);
      await _prefs!.remove(PersistenceKeys.alarmTriggerCount);

      debugPrint('✅ Alarm state cleared');
    } catch (e) {
      debugPrint('❌ Failed to clear alarm state: $e');
    }
  }

  /// Save current dog ID
  Future<void> saveCurrentDogId(String dogId) async {
    await _ensureInitialized();

    try {
      await _prefs!.setString(PersistenceKeys.dogId, dogId);
      debugPrint('💾 Saved current dog ID: $dogId');
    } catch (e) {
      debugPrint('❌ Failed to save dog ID: $e');
    }
  }

  /// Load current dog ID
  Future<String?> loadCurrentDogId() async {
    await _ensureInitialized();

    try {
      final dogId = _prefs!.getString(PersistenceKeys.dogId);
      debugPrint('📖 Loaded dog ID: $dogId');
      return dogId;
    } catch (e) {
      debugPrint('❌ Failed to load dog ID: $e');
      return null;
    }
  }

  /// Save last known sensitivity
  Future<void> saveSensitivity(String sensitivity) async {
    await _ensureInitialized();

    try {
      await _prefs!.setString(PersistenceKeys.lastKnownSensitivity, sensitivity);
      debugPrint('💾 Saved sensitivity: $sensitivity');
    } catch (e) {
      debugPrint('❌ Failed to save sensitivity: $e');
    }
  }

  /// Load last known sensitivity
  Future<String?> loadSensitivity() async {
    await _ensureInitialized();

    try {
      final sensitivity = _prefs!.getString(PersistenceKeys.lastKnownSensitivity);
      debugPrint('📖 Loaded sensitivity: $sensitivity');
      return sensitivity;
    } catch (e) {
      debugPrint('❌ Failed to load sensitivity: $e');
      return null;
    }
  }

  /// Check if alarm was active (useful for app restart)
  Future<bool> wasAlarmActive() async {
    final state = await loadAlarmState();
    return state?.isActive ?? false;
  }

  /// Get activation duration (how long has alarm been active)
  Future<Duration?> getActivationDuration() async {
    final state = await loadAlarmState();
    if (state?.activatedAt == null) return null;

    return DateTime.now().difference(state!.activatedAt!);
  }

  /// Ensure service is initialized
  Future<void> _ensureInitialized() async {
    if (_prefs == null) {
      await initialize();
    }
  }

  /// Clear all persisted data
  Future<void> clearAll() async {
    await _ensureInitialized();

    debugPrint('🗑️  Clearing all persisted data...');

    try {
      await _prefs!.clear();
      debugPrint('✅ All persisted data cleared');
    } catch (e) {
      debugPrint('❌ Failed to clear data: $e');
    }
  }
}

/// Provider for alarm persistence service
final alarmPersistenceServiceProvider = Provider<AlarmPersistenceService>(
  (ref) => AlarmPersistenceService(),
);

/// Provider that loads alarm state on app start
final persistedAlarmStateProvider = FutureProvider<AlarmState?>((ref) async {
  final persistence = ref.watch(alarmPersistenceServiceProvider);
  return await persistence.loadAlarmState();
});
