import 'package:flutter_test/flutter_test.dart';
import 'package:doggy_dogs_car_alarm/services/permission_analyzer.dart';
import 'package:permission_handler/permission_handler.dart';

void main() {
  group('PermissionAnalyzer', () {
    late PermissionAnalyzer analyzer;

    setUp(() {
      analyzer = PermissionAnalyzer();
    });

    group('hasAllRequiredPermissions', () {
      test('returns true when all required permissions are granted', () {
        final statuses = {
          Permission.sensors: PermissionStatus.granted,
          Permission.notification: PermissionStatus.granted,
        };

        expect(analyzer.hasAllRequiredPermissions(statuses), isTrue);
      });

      test('returns false when sensors is denied', () {
        final statuses = {
          Permission.sensors: PermissionStatus.denied,
          Permission.notification: PermissionStatus.granted,
        };

        expect(analyzer.hasAllRequiredPermissions(statuses), isFalse);
      });

      test('returns false when notifications is denied', () {
        final statuses = {
          Permission.sensors: PermissionStatus.granted,
          Permission.notification: PermissionStatus.denied,
        };

        expect(analyzer.hasAllRequiredPermissions(statuses), isFalse);
      });

      test('returns false when both are denied', () {
        final statuses = {
          Permission.sensors: PermissionStatus.denied,
          Permission.notification: PermissionStatus.denied,
        };

        expect(analyzer.hasAllRequiredPermissions(statuses), isFalse);
      });

      test('returns false when sensors is missing from map', () {
        final statuses = {
          Permission.notification: PermissionStatus.granted,
        };

        expect(analyzer.hasAllRequiredPermissions(statuses), isFalse);
      });

      test('returns false when notifications is missing from map', () {
        final statuses = {
          Permission.sensors: PermissionStatus.granted,
        };

        expect(analyzer.hasAllRequiredPermissions(statuses), isFalse);
      });
    });

    group('hasAnyPermanentlyDenied', () {
      test('returns true when one permission is permanently denied', () {
        final statuses = {
          Permission.sensors: PermissionStatus.permanentlyDenied,
          Permission.notification: PermissionStatus.granted,
        };

        expect(analyzer.hasAnyPermanentlyDenied(statuses), isTrue);
      });

      test('returns true when multiple permissions are permanently denied', () {
        final statuses = {
          Permission.sensors: PermissionStatus.permanentlyDenied,
          Permission.notification: PermissionStatus.permanentlyDenied,
        };

        expect(analyzer.hasAnyPermanentlyDenied(statuses), isTrue);
      });

      test('returns false when all permissions are granted', () {
        final statuses = {
          Permission.sensors: PermissionStatus.granted,
          Permission.notification: PermissionStatus.granted,
        };

        expect(analyzer.hasAnyPermanentlyDenied(statuses), isFalse);
      });

      test('returns false when permissions are only denied (not permanently)',
          () {
        final statuses = {
          Permission.sensors: PermissionStatus.denied,
          Permission.notification: PermissionStatus.denied,
        };

        expect(analyzer.hasAnyPermanentlyDenied(statuses), isFalse);
      });

      test('returns false for empty map', () {
        final statuses = <Permission, PermissionStatus>{};

        expect(analyzer.hasAnyPermanentlyDenied(statuses), isFalse);
      });
    });

    group('getPermanentlyDeniedPermissions', () {
      test('returns list of permanently denied permissions', () {
        final statuses = {
          Permission.sensors: PermissionStatus.permanentlyDenied,
          Permission.notification: PermissionStatus.granted,
          Permission.location: PermissionStatus.permanentlyDenied,
        };

        final result = analyzer.getPermanentlyDeniedPermissions(statuses);

        expect(result, hasLength(2));
        expect(result, contains(Permission.sensors));
        expect(result, contains(Permission.location));
      });

      test('returns empty list when no permissions are permanently denied', () {
        final statuses = {
          Permission.sensors: PermissionStatus.granted,
          Permission.notification: PermissionStatus.denied,
        };

        final result = analyzer.getPermanentlyDeniedPermissions(statuses);

        expect(result, isEmpty);
      });
    });

    group('getDeniedPermissions', () {
      test('returns list of denied (but not permanently) permissions', () {
        final statuses = {
          Permission.sensors: PermissionStatus.denied,
          Permission.notification: PermissionStatus.granted,
          Permission.location: PermissionStatus.denied,
          Permission.ignoreBatteryOptimizations:
              PermissionStatus.permanentlyDenied,
        };

        final result = analyzer.getDeniedPermissions(statuses);

        expect(result, hasLength(2));
        expect(result, contains(Permission.sensors));
        expect(result, contains(Permission.location));
        expect(result, isNot(contains(Permission.ignoreBatteryOptimizations)));
      });

      test('returns empty list when no permissions are denied', () {
        final statuses = {
          Permission.sensors: PermissionStatus.granted,
          Permission.notification: PermissionStatus.granted,
        };

        final result = analyzer.getDeniedPermissions(statuses);

        expect(result, isEmpty);
      });
    });

    group('getGrantedPermissions', () {
      test('returns list of granted permissions', () {
        final statuses = {
          Permission.sensors: PermissionStatus.granted,
          Permission.notification: PermissionStatus.granted,
          Permission.location: PermissionStatus.denied,
        };

        final result = analyzer.getGrantedPermissions(statuses);

        expect(result, hasLength(2));
        expect(result, contains(Permission.sensors));
        expect(result, contains(Permission.notification));
      });

      test('returns empty list when no permissions are granted', () {
        final statuses = {
          Permission.sensors: PermissionStatus.denied,
          Permission.notification: PermissionStatus.denied,
        };

        final result = analyzer.getGrantedPermissions(statuses);

        expect(result, isEmpty);
      });
    });

    group('calculateCompletionPercentage', () {
      test('returns 100% when all permissions are granted', () {
        final statuses = {
          Permission.sensors: PermissionStatus.granted,
          Permission.notification: PermissionStatus.granted,
          Permission.location: PermissionStatus.granted,
          Permission.ignoreBatteryOptimizations: PermissionStatus.granted,
        };

        expect(analyzer.calculateCompletionPercentage(statuses), equals(100.0));
      });

      test('returns 50% when half are granted', () {
        final statuses = {
          Permission.sensors: PermissionStatus.granted,
          Permission.notification: PermissionStatus.granted,
          Permission.location: PermissionStatus.denied,
          Permission.ignoreBatteryOptimizations: PermissionStatus.denied,
        };

        expect(analyzer.calculateCompletionPercentage(statuses), equals(50.0));
      });

      test('returns 0% when none are granted', () {
        final statuses = {
          Permission.sensors: PermissionStatus.denied,
          Permission.notification: PermissionStatus.denied,
        };

        expect(analyzer.calculateCompletionPercentage(statuses), equals(0.0));
      });

      test('returns 0% for empty map', () {
        final statuses = <Permission, PermissionStatus>{};

        expect(analyzer.calculateCompletionPercentage(statuses), equals(0.0));
      });

      test('calculates percentage correctly with 3 out of 4 granted', () {
        final statuses = {
          Permission.sensors: PermissionStatus.granted,
          Permission.notification: PermissionStatus.granted,
          Permission.location: PermissionStatus.granted,
          Permission.ignoreBatteryOptimizations: PermissionStatus.denied,
        };

        expect(analyzer.calculateCompletionPercentage(statuses), equals(75.0));
      });
    });

    group('shouldOpenSettings', () {
      test('returns true when any permission is permanently denied', () {
        final statuses = {
          Permission.sensors: PermissionStatus.permanentlyDenied,
          Permission.notification: PermissionStatus.granted,
        };

        expect(analyzer.shouldOpenSettings(statuses), isTrue);
      });

      test('returns false when no permissions are permanently denied', () {
        final statuses = {
          Permission.sensors: PermissionStatus.denied,
          Permission.notification: PermissionStatus.granted,
        };

        expect(analyzer.shouldOpenSettings(statuses), isFalse);
      });
    });

    group('getMissingRequiredPermissions', () {
      test('returns empty list when all required permissions are granted', () {
        final statuses = {
          Permission.sensors: PermissionStatus.granted,
          Permission.notification: PermissionStatus.granted,
          Permission.location: PermissionStatus.denied,
        };

        final result = analyzer.getMissingRequiredPermissions(statuses);

        expect(result, isEmpty);
      });

      test('returns missing required permissions', () {
        final statuses = {
          Permission.sensors: PermissionStatus.denied,
          Permission.notification: PermissionStatus.granted,
        };

        final result = analyzer.getMissingRequiredPermissions(statuses);

        expect(result, hasLength(1));
        expect(result, contains(Permission.sensors));
      });

      test('returns all required permissions when none are granted', () {
        final statuses = {
          Permission.sensors: PermissionStatus.denied,
          Permission.notification: PermissionStatus.denied,
        };

        final result = analyzer.getMissingRequiredPermissions(statuses);

        expect(result, hasLength(2));
        expect(result, contains(Permission.sensors));
        expect(result, contains(Permission.notification));
      });
    });

    group('getMissingRecommendedPermissions', () {
      test('returns empty list when battery optimization is granted', () {
        final statuses = {
          Permission.ignoreBatteryOptimizations: PermissionStatus.granted,
        };

        final result = analyzer.getMissingRecommendedPermissions(statuses);

        expect(result, isEmpty);
      });

      test('returns battery optimization when denied', () {
        final statuses = {
          Permission.ignoreBatteryOptimizations: PermissionStatus.denied,
        };

        final result = analyzer.getMissingRecommendedPermissions(statuses);

        expect(result, hasLength(1));
        expect(result, contains(Permission.ignoreBatteryOptimizations));
      });
    });

    group('getMissingOptionalPermissions', () {
      test('returns empty list when location is granted', () {
        final statuses = {
          Permission.location: PermissionStatus.granted,
        };

        final result = analyzer.getMissingOptionalPermissions(statuses);

        expect(result, isEmpty);
      });

      test('returns location when denied', () {
        final statuses = {
          Permission.location: PermissionStatus.denied,
        };

        final result = analyzer.getMissingOptionalPermissions(statuses);

        expect(result, hasLength(1));
        expect(result, contains(Permission.location));
      });
    });

    group('analyzeRequestResult', () {
      test('creates complete analysis of permission request', () {
        final statuses = {
          Permission.sensors: PermissionStatus.granted,
          Permission.notification: PermissionStatus.denied,
          Permission.location: PermissionStatus.permanentlyDenied,
        };

        final result = analyzer.analyzeRequestResult(statuses);

        expect(result.grantedPermissions, hasLength(1));
        expect(result.grantedPermissions, contains(Permission.sensors));
        expect(result.deniedPermissions, hasLength(1));
        expect(result.deniedPermissions, contains(Permission.notification));
        expect(result.permanentlyDeniedPermissions, hasLength(1));
        expect(
            result.permanentlyDeniedPermissions, contains(Permission.location));
        expect(result.hasAllRequired, isFalse);
        expect(result.shouldOpenSettings, isTrue);
      });

      test('indicates success when all required are granted', () {
        final statuses = {
          Permission.sensors: PermissionStatus.granted,
          Permission.notification: PermissionStatus.granted,
          Permission.location: PermissionStatus.denied,
        };

        final result = analyzer.analyzeRequestResult(statuses);

        expect(result.hasAllRequired, isTrue);
        expect(result.shouldOpenSettings, isFalse);
      });
    });
  });

  group('PermissionRequestResult', () {
    test('hasAnyDenied returns true when there are denied permissions', () {
      const result = PermissionRequestResult(
        grantedPermissions: [],
        deniedPermissions: [Permission.sensors],
        permanentlyDeniedPermissions: [],
        hasAllRequired: false,
        shouldOpenSettings: false,
      );

      expect(result.hasAnyDenied, isTrue);
    });

    test(
        'hasAnyPermanentlyDenied returns true when there are permanently denied',
        () {
      const result = PermissionRequestResult(
        grantedPermissions: [],
        deniedPermissions: [],
        permanentlyDeniedPermissions: [Permission.sensors],
        hasAllRequired: false,
        shouldOpenSettings: true,
      );

      expect(result.hasAnyPermanentlyDenied, isTrue);
    });

    test('allGranted returns true when no permissions are denied', () {
      const result = PermissionRequestResult(
        grantedPermissions: [Permission.sensors, Permission.notification],
        deniedPermissions: [],
        permanentlyDeniedPermissions: [],
        hasAllRequired: true,
        shouldOpenSettings: false,
      );

      expect(result.allGranted, isTrue);
    });

    test('allGranted returns false when some permissions are denied', () {
      const result = PermissionRequestResult(
        grantedPermissions: [Permission.sensors],
        deniedPermissions: [Permission.notification],
        permanentlyDeniedPermissions: [],
        hasAllRequired: false,
        shouldOpenSettings: false,
      );

      expect(result.allGranted, isFalse);
    });

    test('toString includes all relevant information', () {
      const result = PermissionRequestResult(
        grantedPermissions: [Permission.sensors],
        deniedPermissions: [Permission.notification],
        permanentlyDeniedPermissions: [],
        hasAllRequired: false,
        shouldOpenSettings: false,
      );

      final string = result.toString();

      expect(string, contains('PermissionRequestResult'));
      expect(string, contains('granted: 1'));
      expect(string, contains('denied: 1'));
      expect(string, contains('permanentlyDenied: 0'));
      expect(string, contains('hasAllRequired: false'));
      expect(string, contains('shouldOpenSettings: false'));
    });
  });
}
