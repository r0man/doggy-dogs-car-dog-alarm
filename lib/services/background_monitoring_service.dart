import 'dart:async';
import 'package:flutter/material.dart';
import 'package:workmanager/workmanager.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import '../models/alarm_state.dart';
import 'alarm_persistence_service.dart';

/// Background task names
class BackgroundTasks {
  static const monitorSensors = 'monitorSensors';
  static const checkAlarmState = 'checkAlarmState';
}

/// Callback dispatcher for background tasks
/// IMPORTANT: This must be a top-level function
@pragma('vm:entry-point')
void callbackDispatcher() {
  Workmanager().executeTask((task, inputData) async {
    try {
      switch (task) {
        case BackgroundTasks.monitorSensors:
          await _monitorSensorsTask(inputData);
          break;
        case BackgroundTasks.checkAlarmState:
          await _checkAlarmStateTask(inputData);
          break;
        default:
          debugPrint('Unknown background task: $task');
      }
      return Future.value(true);
    } catch (e) {
      debugPrint('Background task error: $e');
      return Future.value(false);
    }
  });
}

/// Monitor sensors task (runs periodically when alarm is active)
Future<void> _monitorSensorsTask(Map<String, dynamic>? inputData) async {
  debugPrint('🔍 Background: Monitoring sensors...');

  // Check if alarm is still active
  final persistence = AlarmPersistenceService();
  final alarmState = await persistence.loadAlarmState();

  if (alarmState == null || !alarmState.isActive) {
    debugPrint('⏹️  Background: Alarm not active, stopping monitoring');
    return;
  }

  // In a real implementation, you would:
  // 1. Access sensors via platform channels
  // 2. Check for motion events
  // 3. Trigger notifications if motion detected

  // For now, we'll log that monitoring is happening
  debugPrint('✅ Background: Monitoring active (${alarmState.mode})');
}

/// Check alarm state task (runs periodically to maintain state)
Future<void> _checkAlarmStateTask(Map<String, dynamic>? inputData) async {
  debugPrint('🔍 Background: Checking alarm state...');

  final persistence = AlarmPersistenceService();
  final alarmState = await persistence.loadAlarmState();

  if (alarmState != null && alarmState.isActive) {
    debugPrint('✅ Background: Alarm still active');

    // Ensure monitoring task is scheduled
    await Workmanager().registerPeriodicTask(
      'sensor-monitoring',
      BackgroundTasks.monitorSensors,
      frequency: const Duration(minutes: 15),
      constraints: Constraints(
        networkType: NetworkType.not_required,
        requiresCharging: false,
      ),
      existingWorkPolicy: ExistingWorkPolicy.keep,
    );
  } else {
    debugPrint('⏹️  Background: Alarm not active');
  }
}

/// Service for managing background monitoring
class BackgroundMonitoringService {
  static final BackgroundMonitoringService _instance =
      BackgroundMonitoringService._internal();

  factory BackgroundMonitoringService() => _instance;

  BackgroundMonitoringService._internal();

  bool _isInitialized = false;
  bool _isMonitoring = false;

  /// Initialize the background monitoring service
  Future<void> initialize() async {
    if (_isInitialized) return;

    debugPrint('🚀 Initializing background monitoring service...');

    try {
      // Initialize workmanager
      await Workmanager().initialize(
        callbackDispatcher,
        isInDebugMode: false, // Set to true for debugging
      );

      _isInitialized = true;
      debugPrint('✅ Background monitoring service initialized');
    } catch (e) {
      debugPrint('❌ Failed to initialize background monitoring: $e');
      rethrow;
    }
  }

  /// Start background monitoring
  Future<void> startMonitoring({
    required AlarmMode mode,
    Duration frequency = const Duration(minutes: 15),
  }) async {
    if (!_isInitialized) {
      await initialize();
    }

    if (_isMonitoring) {
      debugPrint('⚠️  Background monitoring already started');
      return;
    }

    debugPrint('▶️  Starting background monitoring (mode: $mode)...');

    try {
      // Register periodic sensor monitoring task
      await Workmanager().registerPeriodicTask(
        'sensor-monitoring',
        BackgroundTasks.monitorSensors,
        frequency: frequency,
        initialDelay: const Duration(seconds: 5),
        constraints: Constraints(
          networkType: NetworkType.not_required,
          requiresCharging: false,
          requiresBatteryNotLow: false,
        ),
        existingWorkPolicy: ExistingWorkPolicy.replace,
      );

      // Register periodic state check task
      await Workmanager().registerPeriodicTask(
        'alarm-state-check',
        BackgroundTasks.checkAlarmState,
        frequency: const Duration(minutes: 15),
        constraints: Constraints(
          networkType: NetworkType.not_required,
          requiresCharging: false,
        ),
        existingWorkPolicy: ExistingWorkPolicy.replace,
      );

      _isMonitoring = true;
      debugPrint('✅ Background monitoring started');
    } catch (e) {
      debugPrint('❌ Failed to start background monitoring: $e');
      rethrow;
    }
  }

  /// Stop background monitoring
  Future<void> stopMonitoring() async {
    if (!_isMonitoring) return;

    debugPrint('⏹️  Stopping background monitoring...');

    try {
      // Cancel all background tasks
      await Workmanager().cancelByUniqueName('sensor-monitoring');
      await Workmanager().cancelByUniqueName('alarm-state-check');

      _isMonitoring = false;
      debugPrint('✅ Background monitoring stopped');
    } catch (e) {
      debugPrint('❌ Failed to stop background monitoring: $e');
    }
  }

  /// Cancel all background tasks
  Future<void> cancelAll() async {
    debugPrint('🗑️  Cancelling all background tasks...');

    try {
      await Workmanager().cancelAll();
      _isMonitoring = false;
      debugPrint('✅ All background tasks cancelled');
    } catch (e) {
      debugPrint('❌ Failed to cancel background tasks: $e');
    }
  }

  /// Check if background monitoring is active
  bool get isMonitoring => _isMonitoring;

  /// Check if service is initialized
  bool get isInitialized => _isInitialized;
}

/// Provider for background monitoring service
final backgroundMonitoringServiceProvider =
    Provider<BackgroundMonitoringService>(
  (ref) => BackgroundMonitoringService(),
);

/// Provider for monitoring state
final backgroundMonitoringStateProvider = StateProvider<bool>(
  (ref) => false,
);
