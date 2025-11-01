import 'package:permission_handler/permission_handler.dart';

/// Defines strategies for requesting permissions
/// This class contains pure business logic for determining what permissions to request
class PermissionStrategy {
  /// Get list of all permissions that should be requested
  List<Permission> getAllPermissionsToRequest() {
    return [
      Permission.sensors,
      Permission.notification,
      Permission.ignoreBatteryOptimizations,
    ];
  }

  /// Get list of required permissions
  List<Permission> getRequiredPermissions() {
    return [
      Permission.sensors,
      Permission.notification,
    ];
  }

  /// Get list of recommended permissions
  List<Permission> getRecommendedPermissions() {
    return [
      Permission.ignoreBatteryOptimizations,
    ];
  }

  /// Get list of optional permissions
  List<Permission> getOptionalPermissions() {
    return [
      Permission.location,
    ];
  }

  /// Determine if a permission is required
  bool isRequired(Permission permission) {
    return getRequiredPermissions().contains(permission);
  }

  /// Determine if a permission is recommended
  bool isRecommended(Permission permission) {
    return getRecommendedPermissions().contains(permission);
  }

  /// Determine if a permission is optional
  bool isOptional(Permission permission) {
    return getOptionalPermissions().contains(permission);
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

  /// Get permission category (required/recommended/optional)
  PermissionCategory getPermissionCategory(Permission permission) {
    if (isRequired(permission)) {
      return PermissionCategory.required;
    } else if (isRecommended(permission)) {
      return PermissionCategory.recommended;
    } else if (isOptional(permission)) {
      return PermissionCategory.optional;
    }
    return PermissionCategory.unknown;
  }

  /// Get permissions to request based on current statuses
  /// Only returns permissions that should be requested (not already granted or permanently denied)
  List<Permission> getPermissionsToRequest(
      Map<Permission, PermissionStatus> currentStatuses) {
    return getAllPermissionsToRequest().where((permission) {
      final status = currentStatuses[permission];
      // Don't request if already granted or permanently denied
      return status?.isGranted != true && status?.isPermanentlyDenied != true;
    }).toList();
  }
}

/// Category of permission
enum PermissionCategory {
  required,
  recommended,
  optional,
  unknown,
}
