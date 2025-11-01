import 'package:flutter_test/flutter_test.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:doggy_dogs_car_alarm/services/permission_handler_service.dart';
import 'package:doggy_dogs_car_alarm/services/permission_analyzer.dart';
import 'package:doggy_dogs_car_alarm/services/permission_strategy.dart';
import 'package:permission_handler/permission_handler.dart';
import 'mock_permission_checker.dart';

void main() {
  TestWidgetsFlutterBinding.ensureInitialized();

  group('PermissionHandlerService', () {
    late PermissionHandlerService service;

    setUp(() {
      service = PermissionHandlerService();
    });

    test('should create instance successfully', () {
      expect(service, isNotNull);
    });

    test('should return same instance (singleton pattern)', () {
      final service1 = PermissionHandlerService();
      final service2 = PermissionHandlerService();
      expect(service1, equals(service2));
    });

    group('Permission Explanations', () {
      test('should provide explanation for sensor permission', () {
        final explanation =
            service.getPermissionExplanation(Permission.sensors);

        expect(explanation, isNotEmpty);
        expect(explanation, contains('motion'));
        expect(explanation.toLowerCase(), contains('detect'));
      });

      test('should provide explanation for notification permission', () {
        final explanation =
            service.getPermissionExplanation(Permission.notification);

        expect(explanation, isNotEmpty);
        expect(explanation, contains('alert'));
      });

      test('should provide explanation for battery optimization permission',
          () {
        final explanation = service.getPermissionExplanation(
          Permission.ignoreBatteryOptimizations,
        );

        expect(explanation, isNotEmpty);
        expect(explanation.toLowerCase(), contains('background'));
      });

      test('should provide explanation for location permission', () {
        final explanation =
            service.getPermissionExplanation(Permission.location);

        expect(explanation, isNotEmpty);
        expect(explanation.toLowerCase(), contains('pattern'));
        expect(explanation.toLowerCase(), contains('learning'));
      });

      test('should provide default explanation for unknown permission', () {
        final explanation = service.getPermissionExplanation(Permission.camera);

        expect(explanation, isNotEmpty);
        expect(explanation, contains('Required for app functionality'));
      });
    });
  });

  group('PermissionStatusSummary', () {
    test('should calculate hasAllRequired correctly when all granted', () {
      const summary = PermissionStatusSummary(
        sensors: PermissionStatus.granted,
        notifications: PermissionStatus.granted,
        batteryOptimization: PermissionStatus.granted,
        location: PermissionStatus.granted,
      );

      expect(summary.hasAllRequired, isTrue);
    });

    test('should calculate hasAllRequired correctly when sensors denied', () {
      const summary = PermissionStatusSummary(
        sensors: PermissionStatus.denied,
        notifications: PermissionStatus.granted,
        batteryOptimization: PermissionStatus.granted,
        location: PermissionStatus.granted,
      );

      expect(summary.hasAllRequired, isFalse);
    });

    test('should calculate hasAllRequired correctly when notifications denied',
        () {
      const summary = PermissionStatusSummary(
        sensors: PermissionStatus.granted,
        notifications: PermissionStatus.denied,
        batteryOptimization: PermissionStatus.granted,
        location: PermissionStatus.granted,
      );

      expect(summary.hasAllRequired, isFalse);
    });

    test('should calculate hasAllRecommended correctly when all granted', () {
      const summary = PermissionStatusSummary(
        sensors: PermissionStatus.granted,
        notifications: PermissionStatus.granted,
        batteryOptimization: PermissionStatus.granted,
        location: PermissionStatus.granted,
      );

      expect(summary.hasAllRecommended, isTrue);
    });

    test('should calculate hasAllRecommended correctly when battery denied',
        () {
      const summary = PermissionStatusSummary(
        sensors: PermissionStatus.granted,
        notifications: PermissionStatus.granted,
        batteryOptimization: PermissionStatus.denied,
        location: PermissionStatus.granted,
      );

      expect(summary.hasAllRecommended, isFalse);
    });

    test('should calculate hasLocation correctly when granted', () {
      const summary = PermissionStatusSummary(
        sensors: PermissionStatus.granted,
        notifications: PermissionStatus.granted,
        batteryOptimization: PermissionStatus.granted,
        location: PermissionStatus.granted,
      );

      expect(summary.hasLocation, isTrue);
    });

    test('should calculate hasLocation correctly when denied', () {
      const summary = PermissionStatusSummary(
        sensors: PermissionStatus.granted,
        notifications: PermissionStatus.granted,
        batteryOptimization: PermissionStatus.granted,
        location: PermissionStatus.denied,
      );

      expect(summary.hasLocation, isFalse);
    });

    test('should calculate grantedCount correctly with all granted', () {
      const summary = PermissionStatusSummary(
        sensors: PermissionStatus.granted,
        notifications: PermissionStatus.granted,
        batteryOptimization: PermissionStatus.granted,
        location: PermissionStatus.granted,
      );

      expect(summary.grantedCount, equals(4));
    });

    test('should calculate grantedCount correctly with partial grants', () {
      const summary = PermissionStatusSummary(
        sensors: PermissionStatus.granted,
        notifications: PermissionStatus.granted,
        batteryOptimization: PermissionStatus.denied,
        location: PermissionStatus.denied,
      );

      expect(summary.grantedCount, equals(2));
    });

    test('should calculate grantedCount correctly with none granted', () {
      const summary = PermissionStatusSummary(
        sensors: PermissionStatus.denied,
        notifications: PermissionStatus.denied,
        batteryOptimization: PermissionStatus.denied,
        location: PermissionStatus.denied,
      );

      expect(summary.grantedCount, equals(0));
    });

    test('should return correct totalCount', () {
      const summary = PermissionStatusSummary(
        sensors: PermissionStatus.denied,
        notifications: PermissionStatus.denied,
        batteryOptimization: PermissionStatus.denied,
        location: PermissionStatus.denied,
      );

      expect(summary.totalCount, equals(4));
    });

    test('should calculate completionPercentage correctly with all granted',
        () {
      const summary = PermissionStatusSummary(
        sensors: PermissionStatus.granted,
        notifications: PermissionStatus.granted,
        batteryOptimization: PermissionStatus.granted,
        location: PermissionStatus.granted,
      );

      expect(summary.completionPercentage, equals(100.0));
    });

    test('should calculate completionPercentage correctly with 50% granted',
        () {
      const summary = PermissionStatusSummary(
        sensors: PermissionStatus.granted,
        notifications: PermissionStatus.granted,
        batteryOptimization: PermissionStatus.denied,
        location: PermissionStatus.denied,
      );

      expect(summary.completionPercentage, equals(50.0));
    });

    test('should calculate completionPercentage correctly with none granted',
        () {
      const summary = PermissionStatusSummary(
        sensors: PermissionStatus.denied,
        notifications: PermissionStatus.denied,
        batteryOptimization: PermissionStatus.denied,
        location: PermissionStatus.denied,
      );

      expect(summary.completionPercentage, equals(0.0));
    });

    test('should generate correct toString output', () {
      const summary = PermissionStatusSummary(
        sensors: PermissionStatus.granted,
        notifications: PermissionStatus.granted,
        batteryOptimization: PermissionStatus.denied,
        location: PermissionStatus.denied,
      );

      final string = summary.toString();

      expect(string, contains('PermissionStatusSummary'));
      expect(string, contains('sensors:'));
      expect(string, contains('notifications:'));
      expect(string, contains('batteryOptimization:'));
      expect(string, contains('location:'));
      expect(string, contains('progress:'));
      expect(string, contains('2/4'));
      expect(string, contains('50.0%'));
    });
  });

  group('PermissionHandlerService Provider', () {
    test(
        'permissionHandlerServiceProvider returns PermissionHandlerService instance',
        () {
      final container = ProviderContainer();
      addTearDown(container.dispose);

      final service = container.read(permissionHandlerServiceProvider);
      expect(service, isA<PermissionHandlerService>());
    });

    test('permissionHandlerServiceProvider returns singleton instance', () {
      final container = ProviderContainer();
      addTearDown(container.dispose);

      final service1 = container.read(permissionHandlerServiceProvider);
      final service2 = container.read(permissionHandlerServiceProvider);
      expect(service1, equals(service2));
    });
  });

  group('PermissionStatus Edge Cases', () {
    test('should handle permanently denied status in summary', () {
      const summary = PermissionStatusSummary(
        sensors: PermissionStatus.permanentlyDenied,
        notifications: PermissionStatus.granted,
        batteryOptimization: PermissionStatus.granted,
        location: PermissionStatus.granted,
      );

      expect(summary.hasAllRequired, isFalse);
      expect(summary.sensors.isPermanentlyDenied, isTrue);
    });

    test('should handle limited status correctly', () {
      const summary = PermissionStatusSummary(
        sensors: PermissionStatus.limited,
        notifications: PermissionStatus.granted,
        batteryOptimization: PermissionStatus.granted,
        location: PermissionStatus.granted,
      );

      // Limited is not granted, so hasAllRequired should be false
      expect(summary.hasAllRequired, isFalse);
    });

    test('should handle restricted status correctly', () {
      const summary = PermissionStatusSummary(
        sensors: PermissionStatus.restricted,
        notifications: PermissionStatus.granted,
        batteryOptimization: PermissionStatus.granted,
        location: PermissionStatus.granted,
      );

      expect(summary.hasAllRequired, isFalse);
    });
  });

  group('Permission Explanations Integration', () {
    test('getPermissionExplanation works for all permission types', () {
      final service = PermissionHandlerService();

      // Get explanations for all permission types
      final sensorExplanation =
          service.getPermissionExplanation(Permission.sensors);
      final notificationExplanation =
          service.getPermissionExplanation(Permission.notification);
      final batteryExplanation = service
          .getPermissionExplanation(Permission.ignoreBatteryOptimizations);
      final locationExplanation =
          service.getPermissionExplanation(Permission.location);

      // All explanations should be non-empty strings
      expect(sensorExplanation, isNotEmpty);
      expect(notificationExplanation, isNotEmpty);
      expect(batteryExplanation, isNotEmpty);
      expect(locationExplanation, isNotEmpty);
    });
  });

  group('PermissionHandlerService with Mocking', () {
    late MockPermissionChecker mockChecker;
    late PermissionHandlerService service;

    setUp(() {
      mockChecker = MockPermissionChecker();
      service = PermissionHandlerService.forTesting(checker: mockChecker);
    });

    group('hasAllRequiredPermissions', () {
      test('returns true when all required permissions are granted', () async {
        mockChecker.setMultipleStatuses({
          Permission.sensors: PermissionStatus.granted,
          Permission.notification: PermissionStatus.granted,
        });

        final result = await service.hasAllRequiredPermissions();

        expect(result, isTrue);
      });

      test('returns false when sensors is denied', () async {
        mockChecker.setMultipleStatuses({
          Permission.sensors: PermissionStatus.denied,
          Permission.notification: PermissionStatus.granted,
        });

        final result = await service.hasAllRequiredPermissions();

        expect(result, isFalse);
      });

      test('returns false when notifications is denied', () async {
        mockChecker.setMultipleStatuses({
          Permission.sensors: PermissionStatus.granted,
          Permission.notification: PermissionStatus.denied,
        });

        final result = await service.hasAllRequiredPermissions();

        expect(result, isFalse);
      });
    });

    group('requestAllPermissions', () {
      test('requests all permissions from strategy', () async {
        mockChecker.setMultipleRequestResults({
          Permission.sensors: PermissionStatus.granted,
          Permission.notification: PermissionStatus.granted,
          Permission.ignoreBatteryOptimizations: PermissionStatus.granted,
        });

        final result = await service.requestAllPermissions();

        expect(result, hasLength(3));
        expect(result[Permission.sensors], equals(PermissionStatus.granted));
        expect(
            result[Permission.notification], equals(PermissionStatus.granted));
        expect(result[Permission.ignoreBatteryOptimizations],
            equals(PermissionStatus.granted));
        expect(mockChecker.requestedPermissions, hasLength(3));
      });

      test('handles mixed permission results', () async {
        mockChecker.setMultipleRequestResults({
          Permission.sensors: PermissionStatus.granted,
          Permission.notification: PermissionStatus.denied,
          Permission.ignoreBatteryOptimizations:
              PermissionStatus.permanentlyDenied,
        });

        final result = await service.requestAllPermissions();

        expect(result[Permission.sensors], equals(PermissionStatus.granted));
        expect(
            result[Permission.notification], equals(PermissionStatus.denied));
        expect(result[Permission.ignoreBatteryOptimizations],
            equals(PermissionStatus.permanentlyDenied));
      });
    });

    group('hasSensorPermission', () {
      test('returns true when sensor permission is granted', () async {
        mockChecker.setPermissionStatus(
            Permission.sensors, PermissionStatus.granted);

        final result = await service.hasSensorPermission();

        expect(result, isTrue);
      });

      test('returns false when sensor permission is denied', () async {
        mockChecker.setPermissionStatus(
            Permission.sensors, PermissionStatus.denied);

        final result = await service.hasSensorPermission();

        expect(result, isFalse);
      });
    });

    group('requestSensorPermission', () {
      test('requests sensor permission and returns status', () async {
        mockChecker.setPermissionRequestResult(
            Permission.sensors, PermissionStatus.granted);

        final result = await service.requestSensorPermission();

        expect(result, equals(PermissionStatus.granted));
        expect(mockChecker.requestedPermissions, contains(Permission.sensors));
      });

      test('handles denied sensor permission', () async {
        mockChecker.setPermissionRequestResult(
            Permission.sensors, PermissionStatus.denied);

        final result = await service.requestSensorPermission();

        expect(result, equals(PermissionStatus.denied));
      });

      test('handles permanently denied sensor permission', () async {
        mockChecker.setPermissionRequestResult(
            Permission.sensors, PermissionStatus.permanentlyDenied);

        final result = await service.requestSensorPermission();

        expect(result, equals(PermissionStatus.permanentlyDenied));
      });
    });

    group('hasNotificationPermission', () {
      test('returns true when notification permission is granted', () async {
        mockChecker.setPermissionStatus(
            Permission.notification, PermissionStatus.granted);

        final result = await service.hasNotificationPermission();

        expect(result, isTrue);
      });

      test('returns false when notification permission is denied', () async {
        mockChecker.setPermissionStatus(
            Permission.notification, PermissionStatus.denied);

        final result = await service.hasNotificationPermission();

        expect(result, isFalse);
      });
    });

    group('requestNotificationPermission', () {
      test('requests notification permission and returns status', () async {
        mockChecker.setPermissionRequestResult(
            Permission.notification, PermissionStatus.granted);

        final result = await service.requestNotificationPermission();

        expect(result, equals(PermissionStatus.granted));
        expect(mockChecker.requestedPermissions,
            contains(Permission.notification));
      });
    });

    group('isBatteryOptimizationIgnored', () {
      test('returns true when battery optimization is granted', () async {
        mockChecker.setPermissionStatus(
            Permission.ignoreBatteryOptimizations, PermissionStatus.granted);

        final result = await service.isBatteryOptimizationIgnored();

        expect(result, isTrue);
      });

      test('returns false when battery optimization is denied', () async {
        mockChecker.setPermissionStatus(
            Permission.ignoreBatteryOptimizations, PermissionStatus.denied);

        final result = await service.isBatteryOptimizationIgnored();

        expect(result, isFalse);
      });
    });

    group('requestIgnoreBatteryOptimizations', () {
      test('requests battery optimization permission', () async {
        mockChecker.setPermissionRequestResult(
            Permission.ignoreBatteryOptimizations, PermissionStatus.granted);

        final result = await service.requestIgnoreBatteryOptimizations();

        expect(result, equals(PermissionStatus.granted));
        expect(mockChecker.requestedPermissions,
            contains(Permission.ignoreBatteryOptimizations));
      });
    });

    group('hasLocationPermission', () {
      test('returns true when location permission is granted', () async {
        mockChecker.setPermissionStatus(
            Permission.location, PermissionStatus.granted);

        final result = await service.hasLocationPermission();

        expect(result, isTrue);
      });

      test('returns false when location permission is denied', () async {
        mockChecker.setPermissionStatus(
            Permission.location, PermissionStatus.denied);

        final result = await service.hasLocationPermission();

        expect(result, isFalse);
      });
    });

    group('requestLocationPermission', () {
      test('requests location permission and returns status', () async {
        mockChecker.setPermissionRequestResult(
            Permission.location, PermissionStatus.granted);

        final result = await service.requestLocationPermission();

        expect(result, equals(PermissionStatus.granted));
        expect(mockChecker.requestedPermissions, contains(Permission.location));
      });
    });

    group('openAppSettings', () {
      test('calls checker to open app settings', () async {
        final result = await service.openAppSettings();

        expect(result, isTrue);
        expect(mockChecker.settingsOpened, isTrue);
      });
    });

    group('getPermissionSummary', () {
      test('returns summary of all permission statuses', () async {
        mockChecker.setMultipleStatuses({
          Permission.sensors: PermissionStatus.granted,
          Permission.notification: PermissionStatus.granted,
          Permission.ignoreBatteryOptimizations: PermissionStatus.denied,
          Permission.location: PermissionStatus.denied,
        });

        final summary = await service.getPermissionSummary();

        expect(summary.sensors, equals(PermissionStatus.granted));
        expect(summary.notifications, equals(PermissionStatus.granted));
        expect(summary.batteryOptimization, equals(PermissionStatus.denied));
        expect(summary.location, equals(PermissionStatus.denied));
        expect(summary.hasAllRequired, isTrue);
        expect(summary.hasAllRecommended, isFalse);
      });
    });

    group('hasAnyPermanentlyDenied', () {
      test('returns true when sensors is permanently denied', () async {
        mockChecker.setMultipleStatuses({
          Permission.sensors: PermissionStatus.permanentlyDenied,
          Permission.notification: PermissionStatus.granted,
          Permission.ignoreBatteryOptimizations: PermissionStatus.granted,
          Permission.location: PermissionStatus.granted,
        });

        final result = await service.hasAnyPermanentlyDenied();

        expect(result, isTrue);
      });

      test('returns true when notifications is permanently denied', () async {
        mockChecker.setMultipleStatuses({
          Permission.sensors: PermissionStatus.granted,
          Permission.notification: PermissionStatus.permanentlyDenied,
          Permission.ignoreBatteryOptimizations: PermissionStatus.granted,
          Permission.location: PermissionStatus.granted,
        });

        final result = await service.hasAnyPermanentlyDenied();

        expect(result, isTrue);
      });

      test('returns true when location is permanently denied', () async {
        mockChecker.setMultipleStatuses({
          Permission.sensors: PermissionStatus.granted,
          Permission.notification: PermissionStatus.granted,
          Permission.ignoreBatteryOptimizations: PermissionStatus.granted,
          Permission.location: PermissionStatus.permanentlyDenied,
        });

        final result = await service.hasAnyPermanentlyDenied();

        expect(result, isTrue);
      });

      test('returns false when no permissions are permanently denied',
          () async {
        mockChecker.setMultipleStatuses({
          Permission.sensors: PermissionStatus.granted,
          Permission.notification: PermissionStatus.denied,
          Permission.ignoreBatteryOptimizations: PermissionStatus.granted,
          Permission.location: PermissionStatus.denied,
        });

        final result = await service.hasAnyPermanentlyDenied();

        expect(result, isFalse);
      });
    });

    group('Integration with analyzer and strategy', () {
      test('uses analyzer for permission analysis', () async {
        final analyzer = PermissionAnalyzer();
        final customService = PermissionHandlerService.forTesting(
          checker: mockChecker,
          analyzer: analyzer,
        );

        mockChecker.setMultipleStatuses({
          Permission.sensors: PermissionStatus.granted,
          Permission.notification: PermissionStatus.granted,
        });

        final result = await customService.hasAllRequiredPermissions();

        expect(result, isTrue);
      });

      test('uses strategy for permission categorization', () async {
        final strategy = PermissionStrategy();
        final customService = PermissionHandlerService.forTesting(
          checker: mockChecker,
          strategy: strategy,
        );

        final explanation =
            customService.getPermissionExplanation(Permission.sensors);

        expect(explanation, isNotEmpty);
        expect(explanation.toLowerCase(), contains('motion'));
      });
    });
  });
}
