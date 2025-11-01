import 'package:flutter_test/flutter_test.dart';
import 'package:doggy_dogs_car_alarm/services/permission_strategy.dart';
import 'package:permission_handler/permission_handler.dart';

void main() {
  group('PermissionStrategy', () {
    late PermissionStrategy strategy;

    setUp(() {
      strategy = PermissionStrategy();
    });

    group('getAllPermissionsToRequest', () {
      test('returns all permissions that should be requested', () {
        final permissions = strategy.getAllPermissionsToRequest();

        expect(permissions, hasLength(3));
        expect(permissions, contains(Permission.sensors));
        expect(permissions, contains(Permission.notification));
        expect(permissions, contains(Permission.ignoreBatteryOptimizations));
      });
    });

    group('getRequiredPermissions', () {
      test('returns only required permissions', () {
        final permissions = strategy.getRequiredPermissions();

        expect(permissions, hasLength(2));
        expect(permissions, contains(Permission.sensors));
        expect(permissions, contains(Permission.notification));
      });
    });

    group('getRecommendedPermissions', () {
      test('returns only recommended permissions', () {
        final permissions = strategy.getRecommendedPermissions();

        expect(permissions, hasLength(1));
        expect(permissions, contains(Permission.ignoreBatteryOptimizations));
      });
    });

    group('getOptionalPermissions', () {
      test('returns only optional permissions', () {
        final permissions = strategy.getOptionalPermissions();

        expect(permissions, hasLength(1));
        expect(permissions, contains(Permission.location));
      });
    });

    group('isRequired', () {
      test('returns true for sensor permission', () {
        expect(strategy.isRequired(Permission.sensors), isTrue);
      });

      test('returns true for notification permission', () {
        expect(strategy.isRequired(Permission.notification), isTrue);
      });

      test('returns false for battery optimization permission', () {
        expect(strategy.isRequired(Permission.ignoreBatteryOptimizations),
            isFalse);
      });

      test('returns false for location permission', () {
        expect(strategy.isRequired(Permission.location), isFalse);
      });

      test('returns false for camera permission', () {
        expect(strategy.isRequired(Permission.camera), isFalse);
      });
    });

    group('isRecommended', () {
      test('returns true for battery optimization permission', () {
        expect(strategy.isRecommended(Permission.ignoreBatteryOptimizations),
            isTrue);
      });

      test('returns false for sensor permission', () {
        expect(strategy.isRecommended(Permission.sensors), isFalse);
      });

      test('returns false for notification permission', () {
        expect(strategy.isRecommended(Permission.notification), isFalse);
      });

      test('returns false for location permission', () {
        expect(strategy.isRecommended(Permission.location), isFalse);
      });
    });

    group('isOptional', () {
      test('returns true for location permission', () {
        expect(strategy.isOptional(Permission.location), isTrue);
      });

      test('returns false for sensor permission', () {
        expect(strategy.isOptional(Permission.sensors), isFalse);
      });

      test('returns false for notification permission', () {
        expect(strategy.isOptional(Permission.notification), isFalse);
      });

      test('returns false for battery optimization permission', () {
        expect(strategy.isOptional(Permission.ignoreBatteryOptimizations),
            isFalse);
      });
    });

    group('getPermissionExplanation', () {
      test('returns explanation for sensor permission', () {
        final explanation =
            strategy.getPermissionExplanation(Permission.sensors);

        expect(explanation, isNotEmpty);
        expect(explanation.toLowerCase(), contains('motion'));
        expect(explanation.toLowerCase(), contains('detect'));
      });

      test('returns explanation for notification permission', () {
        final explanation =
            strategy.getPermissionExplanation(Permission.notification);

        expect(explanation, isNotEmpty);
        expect(explanation.toLowerCase(), contains('alert'));
      });

      test('returns explanation for battery optimization permission', () {
        final explanation = strategy
            .getPermissionExplanation(Permission.ignoreBatteryOptimizations);

        expect(explanation, isNotEmpty);
        expect(explanation.toLowerCase(), contains('background'));
      });

      test('returns explanation for location permission', () {
        final explanation =
            strategy.getPermissionExplanation(Permission.location);

        expect(explanation, isNotEmpty);
        expect(explanation.toLowerCase(), contains('pattern'));
        expect(explanation.toLowerCase(), contains('learning'));
      });

      test('returns default explanation for unknown permission', () {
        final explanation =
            strategy.getPermissionExplanation(Permission.camera);

        expect(explanation, equals('Required for app functionality'));
      });
    });

    group('getPermissionCategory', () {
      test('returns required for sensor permission', () {
        expect(strategy.getPermissionCategory(Permission.sensors),
            equals(PermissionCategory.required));
      });

      test('returns required for notification permission', () {
        expect(strategy.getPermissionCategory(Permission.notification),
            equals(PermissionCategory.required));
      });

      test('returns recommended for battery optimization permission', () {
        expect(
            strategy
                .getPermissionCategory(Permission.ignoreBatteryOptimizations),
            equals(PermissionCategory.recommended));
      });

      test('returns optional for location permission', () {
        expect(strategy.getPermissionCategory(Permission.location),
            equals(PermissionCategory.optional));
      });

      test('returns unknown for unrecognized permission', () {
        expect(strategy.getPermissionCategory(Permission.camera),
            equals(PermissionCategory.unknown));
      });
    });

    group('getPermissionsToRequest', () {
      test('returns all permissions when none are granted', () {
        final currentStatuses = {
          Permission.sensors: PermissionStatus.denied,
          Permission.notification: PermissionStatus.denied,
          Permission.ignoreBatteryOptimizations: PermissionStatus.denied,
        };

        final result = strategy.getPermissionsToRequest(currentStatuses);

        expect(result, hasLength(3));
        expect(result, contains(Permission.sensors));
        expect(result, contains(Permission.notification));
        expect(result, contains(Permission.ignoreBatteryOptimizations));
      });

      test('excludes already granted permissions', () {
        final currentStatuses = {
          Permission.sensors: PermissionStatus.granted,
          Permission.notification: PermissionStatus.denied,
          Permission.ignoreBatteryOptimizations: PermissionStatus.denied,
        };

        final result = strategy.getPermissionsToRequest(currentStatuses);

        expect(result, hasLength(2));
        expect(result, isNot(contains(Permission.sensors)));
        expect(result, contains(Permission.notification));
        expect(result, contains(Permission.ignoreBatteryOptimizations));
      });

      test('excludes permanently denied permissions', () {
        final currentStatuses = {
          Permission.sensors: PermissionStatus.permanentlyDenied,
          Permission.notification: PermissionStatus.denied,
          Permission.ignoreBatteryOptimizations: PermissionStatus.denied,
        };

        final result = strategy.getPermissionsToRequest(currentStatuses);

        expect(result, hasLength(2));
        expect(result, isNot(contains(Permission.sensors)));
        expect(result, contains(Permission.notification));
        expect(result, contains(Permission.ignoreBatteryOptimizations));
      });

      test('returns empty list when all are granted', () {
        final currentStatuses = {
          Permission.sensors: PermissionStatus.granted,
          Permission.notification: PermissionStatus.granted,
          Permission.ignoreBatteryOptimizations: PermissionStatus.granted,
        };

        final result = strategy.getPermissionsToRequest(currentStatuses);

        expect(result, isEmpty);
      });

      test('handles missing permissions in status map', () {
        final currentStatuses = {
          Permission.sensors: PermissionStatus.granted,
        };

        final result = strategy.getPermissionsToRequest(currentStatuses);

        // Should request notification and battery optimization
        expect(result, hasLength(2));
        expect(result, contains(Permission.notification));
        expect(result, contains(Permission.ignoreBatteryOptimizations));
      });

      test('handles empty status map', () {
        final currentStatuses = <Permission, PermissionStatus>{};

        final result = strategy.getPermissionsToRequest(currentStatuses);

        // Should request all permissions
        expect(result, hasLength(3));
      });
    });

    group('PermissionCategory enum', () {
      test('has all expected values', () {
        expect(PermissionCategory.values, hasLength(4));
        expect(
            PermissionCategory.values, contains(PermissionCategory.required));
        expect(PermissionCategory.values,
            contains(PermissionCategory.recommended));
        expect(
            PermissionCategory.values, contains(PermissionCategory.optional));
        expect(PermissionCategory.values, contains(PermissionCategory.unknown));
      });
    });
  });
}
