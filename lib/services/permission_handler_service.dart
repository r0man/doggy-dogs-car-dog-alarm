import 'package:flutter/material.dart';
import 'package:permission_handler/permission_handler.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:doggy_dogs_car_alarm/services/permission_checker.dart';
import 'package:doggy_dogs_car_alarm/services/permission_analyzer.dart';
import 'package:doggy_dogs_car_alarm/services/permission_strategy.dart';

/// Service for handling app permissions
/// This service orchestrates permission operations using:
/// - PermissionChecker: platform-specific permission operations
/// - PermissionAnalyzer: pure logic for analyzing permission states
/// - PermissionStrategy: business logic for permission requirements
class PermissionHandlerService {
  static final PermissionHandlerService _instance =
      PermissionHandlerService._internal();

  factory PermissionHandlerService() => _instance;

  final PermissionChecker _checker;
  final PermissionAnalyzer _analyzer;
  final PermissionStrategy _strategy;

  PermissionHandlerService._internal()
      : _checker = PlatformPermissionChecker(),
        _analyzer = PermissionAnalyzer(),
        _strategy = PermissionStrategy();

  /// Constructor for testing with custom dependencies
  PermissionHandlerService.forTesting({
    required PermissionChecker checker,
    PermissionAnalyzer? analyzer,
    PermissionStrategy? strategy,
  })  : _checker = checker,
        _analyzer = analyzer ?? PermissionAnalyzer(),
        _strategy = strategy ?? PermissionStrategy();

  /// Check if all required permissions are granted
  Future<bool> hasAllRequiredPermissions() async {
    final statuses = await _getRequiredPermissionStatuses();
    return _analyzer.hasAllRequiredPermissions(statuses);
  }

  /// Request all required permissions
  Future<Map<Permission, PermissionStatus>> requestAllPermissions() async {
    debugPrint('🔐 Requesting all permissions...');

    final permissions = _strategy.getAllPermissionsToRequest();
    final statuses = await _checker.requestPermissions(permissions);

    // Log results
    statuses.forEach((permission, status) {
      final emoji = status.isGranted ? '✅' : '❌';
      debugPrint('$emoji ${permission.toString()}: ${status.toString()}');
    });

    return statuses;
  }

  /// Get current statuses of required permissions
  Future<Map<Permission, PermissionStatus>>
      _getRequiredPermissionStatuses() async {
    final permissions = _strategy.getRequiredPermissions();
    final statuses = <Permission, PermissionStatus>{};

    for (final permission in permissions) {
      statuses[permission] = await _checker.checkPermission(permission);
    }

    return statuses;
  }

  /// Check sensor permission
  Future<bool> hasSensorPermission() async {
    final status = await _checker.checkPermission(Permission.sensors);
    return status.isGranted;
  }

  /// Request sensor permission
  Future<PermissionStatus> requestSensorPermission() async {
    return await _requestPermission(Permission.sensors, 'sensor');
  }

  /// Check notification permission
  Future<bool> hasNotificationPermission() async {
    final status = await _checker.checkPermission(Permission.notification);
    return status.isGranted;
  }

  /// Request notification permission
  Future<PermissionStatus> requestNotificationPermission() async {
    return await _requestPermission(Permission.notification, 'notification');
  }

  /// Check if battery optimizations are ignored (allows background processing)
  Future<bool> isBatteryOptimizationIgnored() async {
    final status =
        await _checker.checkPermission(Permission.ignoreBatteryOptimizations);
    return status.isGranted;
  }

  /// Request to ignore battery optimizations
  Future<PermissionStatus> requestIgnoreBatteryOptimizations() async {
    return await _requestPermission(
      Permission.ignoreBatteryOptimizations,
      'battery optimization',
    );
  }

  /// Check location permission (for pattern learning feature)
  Future<bool> hasLocationPermission() async {
    final status = await _checker.checkPermission(Permission.location);
    return status.isGranted;
  }

  /// Request location permission (for pattern learning feature)
  Future<PermissionStatus> requestLocationPermission() async {
    return await _requestPermission(Permission.location, 'location');
  }

  /// Request a permission and log the result
  Future<PermissionStatus> _requestPermission(
    Permission permission,
    String name,
  ) async {
    debugPrint('🔐 Requesting $name permission...');

    final status = await _checker.requestPermission(permission);

    if (status.isGranted) {
      debugPrint(
          '✅ ${name.substring(0, 1).toUpperCase()}${name.substring(1)} permission granted');
    } else if (status.isDenied) {
      debugPrint(
          '❌ ${name.substring(0, 1).toUpperCase()}${name.substring(1)} permission denied');
    } else if (status.isPermanentlyDenied) {
      debugPrint(
          '⛔ ${name.substring(0, 1).toUpperCase()}${name.substring(1)} permission permanently denied');
    }

    return status;
  }

  /// Open app settings (when permission is permanently denied)
  Future<bool> openAppSettings() async {
    debugPrint('⚙️  Opening app settings...');
    return await _checker.openAppSettings();
  }

  /// Get permission status summary
  Future<PermissionStatusSummary> getPermissionSummary() async {
    final sensors = await _checker.checkPermission(Permission.sensors);
    final notifications =
        await _checker.checkPermission(Permission.notification);
    final batteryOptimization =
        await _checker.checkPermission(Permission.ignoreBatteryOptimizations);
    final location = await _checker.checkPermission(Permission.location);

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
    final statuses = {
      Permission.sensors: summary.sensors,
      Permission.notification: summary.notifications,
      Permission.location: summary.location,
    };

    return _analyzer.hasAnyPermanentlyDenied(statuses);
  }

  /// Get user-friendly permission explanation
  String getPermissionExplanation(Permission permission) {
    return _strategy.getPermissionExplanation(permission);
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

  bool get hasAllRecommended => hasAllRequired && batteryOptimization.isGranted;

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
        '  progress: $grantedCount/$totalCount (${completionPercentage.toStringAsFixed(1)}%)\n'
        ')';
  }
}

/// Provider for permission handler service
final permissionHandlerServiceProvider = Provider<PermissionHandlerService>(
  (ref) => PermissionHandlerService(),
);

/// Provider for permission summary
final permissionSummaryProvider =
    FutureProvider<PermissionStatusSummary>((ref) async {
  final service = ref.watch(permissionHandlerServiceProvider);
  return await service.getPermissionSummary();
});
