import 'package:doggy_dogs_car_alarm/services/permission_checker.dart';
import 'package:permission_handler/permission_handler.dart';

/// Mock implementation of PermissionChecker for testing
class MockPermissionChecker implements PermissionChecker {
  final Map<Permission, PermissionStatus> _statuses = {};
  final Map<Permission, PermissionStatus> _requestResults = {};
  bool settingsOpened = false;
  List<Permission> requestedPermissions = [];

  /// Set the status that will be returned for a permission check
  void setPermissionStatus(Permission permission, PermissionStatus status) {
    _statuses[permission] = status;
  }

  /// Set the result that will be returned for a permission request
  void setPermissionRequestResult(
      Permission permission, PermissionStatus result) {
    _requestResults[permission] = result;
  }

  /// Set multiple permission statuses at once
  void setMultipleStatuses(Map<Permission, PermissionStatus> statuses) {
    _statuses.addAll(statuses);
  }

  /// Set multiple request results at once
  void setMultipleRequestResults(Map<Permission, PermissionStatus> results) {
    _requestResults.addAll(results);
  }

  /// Reset all mock state
  void reset() {
    _statuses.clear();
    _requestResults.clear();
    settingsOpened = false;
    requestedPermissions.clear();
  }

  @override
  Future<PermissionStatus> checkPermission(Permission permission) async {
    return _statuses[permission] ?? PermissionStatus.denied;
  }

  @override
  Future<PermissionStatus> requestPermission(Permission permission) async {
    requestedPermissions.add(permission);
    return _requestResults[permission] ?? PermissionStatus.granted;
  }

  @override
  Future<Map<Permission, PermissionStatus>> requestPermissions(
      List<Permission> permissions) async {
    requestedPermissions.addAll(permissions);
    final results = <Permission, PermissionStatus>{};
    for (final permission in permissions) {
      results[permission] =
          _requestResults[permission] ?? PermissionStatus.granted;
    }
    return results;
  }

  @override
  Future<bool> openAppSettings() async {
    settingsOpened = true;
    return true;
  }
}
