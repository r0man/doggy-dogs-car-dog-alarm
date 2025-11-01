import 'package:permission_handler/permission_handler.dart';

/// Abstract interface for checking and requesting permissions
/// This allows for easy mocking in tests
abstract class PermissionChecker {
  /// Check the status of a permission
  Future<PermissionStatus> checkPermission(Permission permission);

  /// Request a permission
  Future<PermissionStatus> requestPermission(Permission permission);

  /// Request multiple permissions
  Future<Map<Permission, PermissionStatus>> requestPermissions(
      List<Permission> permissions);

  /// Open app settings
  Future<bool> openAppSettings();
}

/// Platform implementation of permission checker
/// This is the only class that directly uses permission_handler
class PlatformPermissionChecker implements PermissionChecker {
  @override
  Future<PermissionStatus> checkPermission(Permission permission) async {
    return await permission.status;
  }

  @override
  Future<PermissionStatus> requestPermission(Permission permission) async {
    return await permission.request();
  }

  @override
  Future<Map<Permission, PermissionStatus>> requestPermissions(
      List<Permission> permissions) async {
    return await permissions.request();
  }

  @override
  Future<bool> openAppSettings() async {
    return await openAppSettings();
  }
}
