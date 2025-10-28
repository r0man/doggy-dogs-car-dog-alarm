import 'package:flutter/material.dart';
import 'package:permission_handler/permission_handler.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';

/// Service for handling app permissions
class PermissionHandlerService {
  static final PermissionHandlerService _instance =
      PermissionHandlerService._internal();

  factory PermissionHandlerService() => _instance;

  PermissionHandlerService._internal();

  /// Check if all required permissions are granted
  Future<bool> hasAllRequiredPermissions() async {
    final sensors = await Permission.sensors.isGranted;
    final notifications = await Permission.notification.isGranted;

    return sensors && notifications;
  }

  /// Request all required permissions
  Future<Map<Permission, PermissionStatus>> requestAllPermissions() async {
    debugPrint('🔐 Requesting all permissions...');

    // Request permissions
    final Map<Permission, PermissionStatus> statuses = await [
      Permission.sensors,
      Permission.notification,
      Permission.ignoreBatteryOptimizations,
    ].request();

    // Log results
    statuses.forEach((permission, status) {
      final emoji = status.isGranted ? '✅' : '❌';
      debugPrint('$emoji ${permission.toString()}: ${status.toString()}');
    });

    return statuses;
  }

  /// Check sensor permission
  Future<bool> hasSensorPermission() async {
    final status = await Permission.sensors.status;
    return status.isGranted;
  }

  /// Request sensor permission
  Future<PermissionStatus> requestSensorPermission() async {
    debugPrint('🔐 Requesting sensor permission...');

    final status = await Permission.sensors.request();

    if (status.isGranted) {
      debugPrint('✅ Sensor permission granted');
    } else if (status.isDenied) {
      debugPrint('❌ Sensor permission denied');
    } else if (status.isPermanentlyDenied) {
      debugPrint('⛔ Sensor permission permanently denied');
    }

    return status;
  }

  /// Check notification permission
  Future<bool> hasNotificationPermission() async {
    final status = await Permission.notification.status;
    return status.isGranted;
  }

  /// Request notification permission
  Future<PermissionStatus> requestNotificationPermission() async {
    debugPrint('🔐 Requesting notification permission...');

    final status = await Permission.notification.request();

    if (status.isGranted) {
      debugPrint('✅ Notification permission granted');
    } else if (status.isDenied) {
      debugPrint('❌ Notification permission denied');
    } else if (status.isPermanentlyDenied) {
      debugPrint('⛔ Notification permission permanently denied');
    }

    return status;
  }

  /// Check if battery optimizations are ignored (allows background processing)
  Future<bool> isBatteryOptimizationIgnored() async {
    final status = await Permission.ignoreBatteryOptimizations.status;
    return status.isGranted;
  }

  /// Request to ignore battery optimizations
  Future<PermissionStatus> requestIgnoreBatteryOptimizations() async {
    debugPrint('🔐 Requesting to ignore battery optimizations...');

    final status = await Permission.ignoreBatteryOptimizations.request();

    if (status.isGranted) {
      debugPrint('✅ Battery optimization permission granted');
    } else {
      debugPrint('❌ Battery optimization permission denied');
    }

    return status;
  }

  /// Check location permission (for pattern learning feature)
  Future<bool> hasLocationPermission() async {
    final status = await Permission.location.status;
    return status.isGranted;
  }

  /// Request location permission (for pattern learning feature)
  Future<PermissionStatus> requestLocationPermission() async {
    debugPrint('🔐 Requesting location permission...');

    final status = await Permission.location.request();

    if (status.isGranted) {
      debugPrint('✅ Location permission granted');
    } else if (status.isDenied) {
      debugPrint('❌ Location permission denied');
    } else if (status.isPermanentlyDenied) {
      debugPrint('⛔ Location permission permanently denied');
    }

    return status;
  }

  /// Open app settings (when permission is permanently denied)
  Future<bool> openAppSettings() async {
    debugPrint('⚙️  Opening app settings...');
    return await openAppSettings();
  }

  /// Get permission status summary
  Future<PermissionStatusSummary> getPermissionSummary() async {
    final sensors = await Permission.sensors.status;
    final notifications = await Permission.notification.status;
    final batteryOptimization = await Permission.ignoreBatteryOptimizations.status;
    final location = await Permission.location.status;

    return PermissionStatusSummary(
      sensors: sensors,
      notifications: notifications,
      batteryOptimization: batteryOptimization,
      location: location,
    );
  }

  /// Check if any permission is permanently denied
  Future<bool> hasAnyPermanentlyDenied() async {
    final summary = await getPermissionSummary();

    return summary.sensors.isPermanentlyDenied ||
        summary.notifications.isPermanentlyDenied ||
        summary.location.isPermanentlyDenied;
  }

  /// Get user-friendly permission explanation
  String getPermissionExplanation(Permission permission) {
    switch (permission) {
      case Permission.sensors:
        return 'Required to detect motion when your car is being tampered with';
      case Permission.notification:
        return 'Required to alert you when the alarm is triggered';
      case Permission.ignoreBatteryOptimizations:
        return 'Recommended to ensure the alarm works in the background';
      case Permission.location:
        return 'Optional: Used for pattern learning to adjust sensitivity based on parking location';
      default:
        return 'Required for app functionality';
    }
  }
}

/// Summary of all permission statuses
class PermissionStatusSummary {
  final PermissionStatus sensors;
  final PermissionStatus notifications;
  final PermissionStatus batteryOptimization;
  final PermissionStatus location;

  const PermissionStatusSummary({
    required this.sensors,
    required this.notifications,
    required this.batteryOptimization,
    required this.location,
  });

  bool get hasAllRequired => sensors.isGranted && notifications.isGranted;

  bool get hasAllRecommended =>
      hasAllRequired && batteryOptimization.isGranted;

  bool get hasLocation => location.isGranted;

  int get grantedCount {
    int count = 0;
    if (sensors.isGranted) count++;
    if (notifications.isGranted) count++;
    if (batteryOptimization.isGranted) count++;
    if (location.isGranted) count++;
    return count;
  }

  int get totalCount => 4;

  double get completionPercentage => (grantedCount / totalCount) * 100;

  @override
  String toString() {
    return 'PermissionStatusSummary(\n'
        '  sensors: $sensors\n'
        '  notifications: $notifications\n'
        '  batteryOptimization: $batteryOptimization\n'
        '  location: $location\n'
        '  progress: ${grantedCount}/${totalCount} (${completionPercentage.toStringAsFixed(1)}%)\n'
        ')';
  }
}

/// Provider for permission handler service
final permissionHandlerServiceProvider = Provider<PermissionHandlerService>(
  (ref) => PermissionHandlerService(),
);

/// Provider for permission summary
final permissionSummaryProvider = FutureProvider<PermissionStatusSummary>((ref) async {
  final service = ref.watch(permissionHandlerServiceProvider);
  return await service.getPermissionSummary();
});
