import 'package:flutter/material.dart';
import 'package:flutter_local_notifications/flutter_local_notifications.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import '../models/alarm_state.dart';
import '../models/sensor_data.dart';

/// Notification channels
class NotificationChannels {
  static const alarmTriggered = 'alarm_triggered';
  static const alarmStatus = 'alarm_status';
  static const dogReminders = 'dog_reminders';
}

/// Service for managing local notifications
class NotificationService {
  static final NotificationService _instance = NotificationService._internal();

  factory NotificationService() => _instance;

  NotificationService._internal();

  final FlutterLocalNotificationsPlugin _notifications =
      FlutterLocalNotificationsPlugin();

  bool _isInitialized = false;

  /// Initialize the notification service
  Future<void> initialize() async {
    if (_isInitialized) return;

    debugPrint('🔔 Initializing notification service...');

    // Android initialization settings
    const androidSettings =
        AndroidInitializationSettings('@mipmap/ic_launcher');

    // iOS initialization settings
    const iosSettings = DarwinInitializationSettings(
      requestAlertPermission: true,
      requestBadgePermission: true,
      requestSoundPermission: true,
    );

    const initSettings = InitializationSettings(
      android: androidSettings,
      iOS: iosSettings,
    );

    try {
      final initialized = await _notifications.initialize(
        initSettings,
        onDidReceiveNotificationResponse: _handleNotificationResponse,
      );

      if (initialized == true) {
        await _createNotificationChannels();
        _isInitialized = true;
        debugPrint('✅ Notification service initialized');
      } else {
        debugPrint('⚠️  Notification initialization returned false');
      }
    } catch (e) {
      debugPrint('❌ Failed to initialize notifications: $e');
    }
  }

  /// Create notification channels (Android)
  Future<void> _createNotificationChannels() async {
    // Alarm triggered channel (high priority)
    // Note: Bark sounds are played through BarkAudioService, not notification sounds
    const alarmChannel = AndroidNotificationChannel(
      NotificationChannels.alarmTriggered,
      'Alarm Alerts',
      description: 'Notifications when your car alarm is triggered',
      importance: Importance.max,
      enableVibration: true,
      enableLights: true,
      playSound: false, // Bark sounds handled by BarkAudioService
    );

    // Alarm status channel (medium priority)
    const statusChannel = AndroidNotificationChannel(
      NotificationChannels.alarmStatus,
      'Alarm Status',
      description: 'Status updates about your car alarm',
      importance: Importance.defaultImportance,
      enableVibration: false,
    );

    // Dog reminders channel (low priority)
    const remindersChannel = AndroidNotificationChannel(
      NotificationChannels.dogReminders,
      'Dog Care Reminders',
      description: 'Reminders to feed and care for your guard dog',
      importance: Importance.low,
    );

    final plugin = _notifications.resolvePlatformSpecificImplementation<
        AndroidFlutterLocalNotificationsPlugin>();

    if (plugin != null) {
      await plugin.createNotificationChannel(alarmChannel);
      await plugin.createNotificationChannel(statusChannel);
      await plugin.createNotificationChannel(remindersChannel);
    }
  }

  /// Handle notification tap/response
  void _handleNotificationResponse(NotificationResponse response) {
    debugPrint('Notification tapped: ${response.payload}');

    // TODO: Navigate to appropriate screen based on payload
    // This would typically use a navigator key or routing logic
  }

  /// Request notification permissions (iOS/Android 13+)
  Future<bool> requestPermissions() async {
    if (!_isInitialized) {
      await initialize();
    }

    final androidPlugin = _notifications.resolvePlatformSpecificImplementation<
        AndroidFlutterLocalNotificationsPlugin>();

    final iosPlugin = _notifications.resolvePlatformSpecificImplementation<
        IOSFlutterLocalNotificationsPlugin>();

    // Request Android 13+ notification permissions
    if (androidPlugin != null) {
      final granted = await androidPlugin.requestNotificationsPermission();
      if (granted != true) {
        debugPrint('⚠️  Android notification permissions denied');
        return false;
      }
    }

    // Request iOS permissions
    if (iosPlugin != null) {
      final granted = await iosPlugin.requestPermissions(
        alert: true,
        badge: true,
        sound: true,
      );
      if (granted != true) {
        debugPrint('⚠️  iOS notification permissions denied');
        return false;
      }
    }

    debugPrint('✅ Notification permissions granted');
    return true;
  }

  /// Show alarm triggered notification
  Future<void> showAlarmTriggered({
    required MotionType motionType,
    required double intensity,
    String? dogName,
  }) async {
    if (!_isInitialized) {
      await initialize();
    }

    final title = '🚨 ${dogName ?? 'Guard Dog'} Alert!';
    final body = _getAlarmMessage(motionType, intensity);

    const androidDetails = AndroidNotificationDetails(
      NotificationChannels.alarmTriggered,
      'Alarm Alerts',
      channelDescription: 'Notifications when your car alarm is triggered',
      importance: Importance.max,
      priority: Priority.max,
      enableVibration: true,
      enableLights: true,
      playSound: false, // Bark sounds handled by BarkAudioService
      category: AndroidNotificationCategory.alarm,
      ongoing: true, // Make it persistent until acknowledged
      autoCancel: false,
      actions: [
        AndroidNotificationAction(
          'acknowledge',
          'Acknowledge',
          showsUserInterface: true,
        ),
        AndroidNotificationAction(
          'deactivate',
          'Deactivate Alarm',
          showsUserInterface: true,
        ),
      ],
    );

    const iosDetails = DarwinNotificationDetails(
      presentAlert: true,
      presentBadge: true,
      presentSound: false, // Bark sounds handled by BarkAudioService
      interruptionLevel: InterruptionLevel.critical,
    );

    const details = NotificationDetails(
      android: androidDetails,
      iOS: iosDetails,
    );

    try {
      await _notifications.show(
        1, // Use ID 1 for alarm triggered notifications
        title,
        body,
        details,
        payload: 'alarm_triggered',
      );

      debugPrint('🔔 Alarm triggered notification sent');
    } catch (e) {
      debugPrint('❌ Failed to show alarm notification: $e');
    }
  }

  /// Show alarm activated notification
  Future<void> showAlarmActivated({
    required AlarmMode mode,
    String? dogName,
  }) async {
    if (!_isInitialized) {
      await initialize();
    }

    const title = '✅ Alarm Activated';
    final body =
        '${dogName ?? 'Your guard dog'} is now watching your car in ${mode.name} mode';

    const androidDetails = AndroidNotificationDetails(
      NotificationChannels.alarmStatus,
      'Alarm Status',
      importance: Importance.defaultImportance,
      priority: Priority.defaultPriority,
      ongoing: true, // Keep showing while alarm is active
      autoCancel: false,
      actions: [
        AndroidNotificationAction(
          'deactivate',
          'Deactivate',
          showsUserInterface: true,
        ),
      ],
    );

    const iosDetails = DarwinNotificationDetails(
      presentAlert: true,
      presentBadge: true,
    );

    const details = NotificationDetails(
      android: androidDetails,
      iOS: iosDetails,
    );

    try {
      await _notifications.show(
        2, // Use ID 2 for status notifications
        title,
        body,
        details,
        payload: 'alarm_active',
      );

      debugPrint('🔔 Alarm activated notification sent');
    } catch (e) {
      debugPrint('❌ Failed to show activation notification: $e');
    }
  }

  /// Show alarm deactivated notification
  Future<void> showAlarmDeactivated({String? dogName}) async {
    if (!_isInitialized) {
      await initialize();
    }

    // Cancel all existing alarm notifications
    await _notifications.cancel(1); // Triggered notification
    await _notifications.cancel(2); // Status notification

    const title = '⏹️  Alarm Deactivated';
    final body = '${dogName ?? 'Your guard dog'} is now resting';

    const androidDetails = AndroidNotificationDetails(
      NotificationChannels.alarmStatus,
      'Alarm Status',
      importance: Importance.defaultImportance,
      priority: Priority.defaultPriority,
      autoCancel: true,
    );

    const iosDetails = DarwinNotificationDetails(
      presentAlert: true,
      presentBadge: true,
    );

    const details = NotificationDetails(
      android: androidDetails,
      iOS: iosDetails,
    );

    try {
      await _notifications.show(
        3,
        title,
        body,
        details,
        payload: 'alarm_deactivated',
      );

      debugPrint('🔔 Alarm deactivated notification sent');
    } catch (e) {
      debugPrint('❌ Failed to show deactivation notification: $e');
    }
  }

  /// Show dog care reminder
  Future<void> showDogReminder({
    required String title,
    required String message,
  }) async {
    if (!_isInitialized) {
      await initialize();
    }

    const androidDetails = AndroidNotificationDetails(
      NotificationChannels.dogReminders,
      'Dog Care Reminders',
      importance: Importance.low,
      priority: Priority.low,
    );

    const iosDetails = DarwinNotificationDetails(
      presentAlert: true,
      presentBadge: true,
    );

    const details = NotificationDetails(
      android: androidDetails,
      iOS: iosDetails,
    );

    try {
      await _notifications.show(
        100, // Use 100+ for reminders
        title,
        message,
        details,
        payload: 'dog_reminder',
      );

      debugPrint('🔔 Dog reminder sent: $title');
    } catch (e) {
      debugPrint('❌ Failed to show reminder: $e');
    }
  }

  /// Cancel a specific notification
  Future<void> cancelNotification(int id) async {
    await _notifications.cancel(id);
  }

  /// Cancel all notifications
  Future<void> cancelAllNotifications() async {
    await _notifications.cancelAll();
  }

  /// Get appropriate message for motion type and intensity
  String _getAlarmMessage(MotionType type, double intensity) {
    final intensityText = intensity > 0.8
        ? 'STRONG'
        : intensity > 0.5
            ? 'MODERATE'
            : 'LIGHT';

    switch (type) {
      case MotionType.impact:
        return '$intensityText IMPACT detected! Someone may have hit your car!';
      case MotionType.shake:
        return '$intensityText SHAKING detected! Someone may be tampering with your car!';
      case MotionType.tilt:
        return '$intensityText TILT detected! Your car may be moving!';
      case MotionType.subtle:
        return 'Motion detected near your car';
    }
  }

  /// Check if notifications are enabled
  Future<bool> areNotificationsEnabled() async {
    if (!_isInitialized) {
      await initialize();
    }

    // Check Android
    final androidPlugin = _notifications.resolvePlatformSpecificImplementation<
        AndroidFlutterLocalNotificationsPlugin>();

    if (androidPlugin != null) {
      final enabled = await androidPlugin.areNotificationsEnabled();
      return enabled ?? false;
    }

    // iOS always returns true if permissions were granted
    return true;
  }
}

/// Provider for notification service
final notificationServiceProvider = Provider<NotificationService>(
  (ref) => NotificationService(),
);
