import 'package:permission_handler/permission_handler.dart';

/// Analyzes permission states and provides insights
/// This class contains pure logic that is easily testable
class PermissionAnalyzer {
  /// Check if all required permissions are granted
  bool hasAllRequiredPermissions(Map<Permission, PermissionStatus> statuses) {
    final sensors = statuses[Permission.sensors];
    final notifications = statuses[Permission.notification];

    return sensors?.isGranted == true && notifications?.isGranted == true;
  }

  /// Check if any permission is permanently denied
  bool hasAnyPermanentlyDenied(Map<Permission, PermissionStatus> statuses) {
    return statuses.values.any((status) => status.isPermanentlyDenied);
  }

  /// Get permissions that are permanently denied
  List<Permission> getPermanentlyDeniedPermissions(
      Map<Permission, PermissionStatus> statuses) {
    return statuses.entries
        .where((entry) => entry.value.isPermanentlyDenied)
        .map((entry) => entry.key)
        .toList();
  }

  /// Get permissions that are denied (but not permanently)
  List<Permission> getDeniedPermissions(
      Map<Permission, PermissionStatus> statuses) {
    return statuses.entries
        .where((entry) => entry.value.isDenied)
        .map((entry) => entry.key)
        .toList();
  }

  /// Get permissions that are granted
  List<Permission> getGrantedPermissions(
      Map<Permission, PermissionStatus> statuses) {
    return statuses.entries
        .where((entry) => entry.value.isGranted)
        .map((entry) => entry.key)
        .toList();
  }

  /// Calculate completion percentage
  double calculateCompletionPercentage(
      Map<Permission, PermissionStatus> statuses) {
    if (statuses.isEmpty) return 0.0;

    final granted = statuses.values.where((status) => status.isGranted).length;
    return (granted / statuses.length) * 100;
  }

  /// Determine if settings should be opened based on permission states
  bool shouldOpenSettings(Map<Permission, PermissionStatus> statuses) {
    return hasAnyPermanentlyDenied(statuses);
  }

  /// Get missing required permissions
  List<Permission> getMissingRequiredPermissions(
      Map<Permission, PermissionStatus> statuses) {
    final requiredPermissions = [
      Permission.sensors,
      Permission.notification,
    ];

    return requiredPermissions
        .where((permission) => statuses[permission]?.isGranted != true)
        .toList();
  }

  /// Get missing recommended permissions
  List<Permission> getMissingRecommendedPermissions(
      Map<Permission, PermissionStatus> statuses) {
    final recommendedPermissions = [
      Permission.ignoreBatteryOptimizations,
    ];

    return recommendedPermissions
        .where((permission) => statuses[permission]?.isGranted != true)
        .toList();
  }

  /// Get missing optional permissions
  List<Permission> getMissingOptionalPermissions(
      Map<Permission, PermissionStatus> statuses) {
    final optionalPermissions = [
      Permission.location,
    ];

    return optionalPermissions
        .where((permission) => statuses[permission]?.isGranted != true)
        .toList();
  }

  /// Analyze permission request result
  PermissionRequestResult analyzeRequestResult(
      Map<Permission, PermissionStatus> statuses) {
    final granted = getGrantedPermissions(statuses);
    final denied = getDeniedPermissions(statuses);
    final permanentlyDenied = getPermanentlyDeniedPermissions(statuses);
    final allRequired = hasAllRequiredPermissions(statuses);

    return PermissionRequestResult(
      grantedPermissions: granted,
      deniedPermissions: denied,
      permanentlyDeniedPermissions: permanentlyDenied,
      hasAllRequired: allRequired,
      shouldOpenSettings: shouldOpenSettings(statuses),
    );
  }
}

/// Result of analyzing a permission request
class PermissionRequestResult {
  final List<Permission> grantedPermissions;
  final List<Permission> deniedPermissions;
  final List<Permission> permanentlyDeniedPermissions;
  final bool hasAllRequired;
  final bool shouldOpenSettings;

  const PermissionRequestResult({
    required this.grantedPermissions,
    required this.deniedPermissions,
    required this.permanentlyDeniedPermissions,
    required this.hasAllRequired,
    required this.shouldOpenSettings,
  });

  bool get hasAnyDenied => deniedPermissions.isNotEmpty;
  bool get hasAnyPermanentlyDenied => permanentlyDeniedPermissions.isNotEmpty;
  bool get allGranted =>
      deniedPermissions.isEmpty && permanentlyDeniedPermissions.isEmpty;

  @override
  String toString() {
    return 'PermissionRequestResult(\n'
        '  granted: ${grantedPermissions.length}\n'
        '  denied: ${deniedPermissions.length}\n'
        '  permanentlyDenied: ${permanentlyDeniedPermissions.length}\n'
        '  hasAllRequired: $hasAllRequired\n'
        '  shouldOpenSettings: $shouldOpenSettings\n'
        ')';
  }
}
