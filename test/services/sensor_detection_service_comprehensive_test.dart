import 'dart:async';
import 'package:flutter_test/flutter_test.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:doggy_dogs_car_alarm/services/sensor_detection_service.dart';
import 'package:doggy_dogs_car_alarm/models/sensor_data.dart';
import 'package:doggy_dogs_car_alarm/services/app_settings_service.dart';
import 'package:doggy_dogs_car_alarm/services/unlock_code_service.dart';
import 'package:doggy_dogs_car_alarm/models/app_settings.dart';
import 'package:shared_preferences/shared_preferences.dart';

void main() {
  TestWidgetsFlutterBinding.ensureInitialized();

  group('SensorDetectionService Constructor and Getters', () {
    test('creates instance with default medium sensitivity', () {
      final service = SensorDetectionService();
      expect(service.sensitivity, AlarmSensitivity.medium);
      expect(service.isMonitoring, isFalse);
    });

    test('creates instance with custom sensitivity', () {
      final serviceLow =
          SensorDetectionService(sensitivity: AlarmSensitivity.low);
      expect(serviceLow.sensitivity, AlarmSensitivity.low);

      final serviceHigh =
          SensorDetectionService(sensitivity: AlarmSensitivity.high);
      expect(serviceHigh.sensitivity, AlarmSensitivity.high);

      final serviceVeryHigh =
          SensorDetectionService(sensitivity: AlarmSensitivity.veryHigh);
      expect(serviceVeryHigh.sensitivity, AlarmSensitivity.veryHigh);
    });

    test('motionEvents stream is available', () {
      final service = SensorDetectionService();
      expect(service.motionEvents, isA<Stream<MotionEvent>>());
      service.dispose();
    });

    test('isMonitoring returns false initially', () {
      final service = SensorDetectionService();
      expect(service.isMonitoring, isFalse);
      service.dispose();
    });
  });

  group('SensorDetectionService recalibrate', () {
    test('recalibrate method completes successfully', () async {
      final service = SensorDetectionService();
      await expectLater(service.recalibrate(), completes);
      service.dispose();
    });
  });

  group('SensorDetectionService dispose', () {
    test('dispose cleans up resources', () {
      final service = SensorDetectionService();
      expect(() => service.dispose(), returnsNormally);
    });

    test('dispose can be called multiple times safely', () {
      final service = SensorDetectionService();
      service.dispose();
      expect(() => service.dispose(), returnsNormally);
    });
  });

  group('SensorDetectionService Providers', () {
    late SharedPreferences prefs;

    setUp(() async {
      SharedPreferences.setMockInitialValues({});
      prefs = await SharedPreferences.getInstance();
    });

    test(
        'sensorDetectionServiceProvider creates service with given sensitivity',
        () {
      final container = ProviderContainer(
        overrides: [
          sharedPreferencesProvider.overrideWithValue(prefs),
        ],
      );
      addTearDown(container.dispose);

      final service = container.read(
        sensorDetectionServiceProvider(AlarmSensitivity.high),
      );
      expect(service, isA<SensorDetectionService>());
      expect(service.sensitivity, AlarmSensitivity.high);
    });

    test(
        'sensorDetectionServiceProvider creates different instances for different sensitivities',
        () {
      final container = ProviderContainer(
        overrides: [
          sharedPreferencesProvider.overrideWithValue(prefs),
        ],
      );
      addTearDown(container.dispose);

      final serviceLow = container.read(
        sensorDetectionServiceProvider(AlarmSensitivity.low),
      );
      final serviceHigh = container.read(
        sensorDetectionServiceProvider(AlarmSensitivity.high),
      );

      expect(serviceLow, isA<SensorDetectionService>());
      expect(serviceHigh, isA<SensorDetectionService>());
      expect(serviceLow.sensitivity, AlarmSensitivity.low);
      expect(serviceHigh.sensitivity, AlarmSensitivity.high);
    });

    test('alarmSensitivityProvider returns correct sensitivity from settings',
        () {
      const testSettings = AppSettings(sensitivityLevel: 'high');

      final container = ProviderContainer(
        overrides: [
          sharedPreferencesProvider.overrideWithValue(prefs),
          appSettingsProvider.overrideWith(
            (ref) => AppSettingsNotifier(
              AppSettingsService(prefs),
            )..state = testSettings,
          ),
        ],
      );
      addTearDown(container.dispose);

      final sensitivity = container.read(alarmSensitivityProvider);
      expect(sensitivity, AlarmSensitivity.high);
    });

    test('alarmSensitivityProvider returns low sensitivity', () {
      const testSettings = AppSettings(sensitivityLevel: 'low');

      final container = ProviderContainer(
        overrides: [
          sharedPreferencesProvider.overrideWithValue(prefs),
          appSettingsProvider.overrideWith(
            (ref) => AppSettingsNotifier(
              AppSettingsService(prefs),
            )..state = testSettings,
          ),
        ],
      );
      addTearDown(container.dispose);

      final sensitivity = container.read(alarmSensitivityProvider);
      expect(sensitivity, AlarmSensitivity.low);
    });

    test('alarmSensitivityProvider returns veryHigh sensitivity', () {
      const testSettings = AppSettings(sensitivityLevel: 'veryHigh');

      final container = ProviderContainer(
        overrides: [
          sharedPreferencesProvider.overrideWithValue(prefs),
          appSettingsProvider.overrideWith(
            (ref) => AppSettingsNotifier(
              AppSettingsService(prefs),
            )..state = testSettings,
          ),
        ],
      );
      addTearDown(container.dispose);

      final sensitivity = container.read(alarmSensitivityProvider);
      expect(sensitivity, AlarmSensitivity.veryHigh);
    });

    test('alarmSensitivityProvider defaults to medium for unknown sensitivity',
        () {
      const testSettings = AppSettings(sensitivityLevel: 'invalid');

      final container = ProviderContainer(
        overrides: [
          sharedPreferencesProvider.overrideWithValue(prefs),
          appSettingsProvider.overrideWith(
            (ref) => AppSettingsNotifier(
              AppSettingsService(prefs),
            )..state = testSettings,
          ),
        ],
      );
      addTearDown(container.dispose);

      final sensitivity = container.read(alarmSensitivityProvider);
      expect(sensitivity, AlarmSensitivity.medium);
    });

    test('sensorMonitoringProvider has initial value of false', () {
      final container = ProviderContainer();
      addTearDown(container.dispose);

      final isMonitoring = container.read(sensorMonitoringProvider);
      expect(isMonitoring, isFalse);
    });

    test('sensorMonitoringProvider can be updated', () {
      final container = ProviderContainer();
      addTearDown(container.dispose);

      expect(container.read(sensorMonitoringProvider), isFalse);

      container.read(sensorMonitoringProvider.notifier).state = true;
      expect(container.read(sensorMonitoringProvider), isTrue);

      container.read(sensorMonitoringProvider.notifier).state = false;
      expect(container.read(sensorMonitoringProvider), isFalse);
    });
  });

  group('SensorDetectionService Different Sensitivities', () {
    test('low sensitivity has higher threshold', () {
      final service = SensorDetectionService(sensitivity: AlarmSensitivity.low);
      expect(
          service.sensitivity.accelerometerThreshold,
          greaterThan(
            SensorDetectionService(sensitivity: AlarmSensitivity.medium)
                .sensitivity
                .accelerometerThreshold,
          ));
      service.dispose();
    });

    test('high sensitivity has lower threshold', () {
      final serviceMedium =
          SensorDetectionService(sensitivity: AlarmSensitivity.medium);
      final serviceHigh =
          SensorDetectionService(sensitivity: AlarmSensitivity.high);

      expect(
          serviceHigh.sensitivity.accelerometerThreshold,
          lessThan(
            serviceMedium.sensitivity.accelerometerThreshold,
          ));

      serviceMedium.dispose();
      serviceHigh.dispose();
    });

    test('veryHigh sensitivity has lowest threshold', () {
      final serviceHigh =
          SensorDetectionService(sensitivity: AlarmSensitivity.high);
      final serviceVeryHigh =
          SensorDetectionService(sensitivity: AlarmSensitivity.veryHigh);

      expect(
          serviceVeryHigh.sensitivity.accelerometerThreshold,
          lessThan(
            serviceHigh.sensitivity.accelerometerThreshold,
          ));

      serviceHigh.dispose();
      serviceVeryHigh.dispose();
    });
  });

  group('SensorDetectionService Motion Event Stream', () {
    test('motion event stream can be listened to', () async {
      final service = SensorDetectionService();

      final subscription = service.motionEvents.listen((_) {});
      expect(subscription, isA<StreamSubscription<MotionEvent>>());

      await subscription.cancel();
      service.dispose();
    });

    test('multiple listeners can subscribe to motion events', () async {
      final service = SensorDetectionService();

      final subscription1 = service.motionEvents.listen((_) {});
      final subscription2 = service.motionEvents.listen((_) {});

      expect(subscription1, isA<StreamSubscription<MotionEvent>>());
      expect(subscription2, isA<StreamSubscription<MotionEvent>>());

      await subscription1.cancel();
      await subscription2.cancel();
      service.dispose();
    });
  });

  group('SensorDetectionService stopMonitoring', () {
    test('stopMonitoring when not monitoring returns without error', () async {
      final service = SensorDetectionService();
      expect(service.isMonitoring, isFalse);
      await expectLater(service.stopMonitoring(), completes);
      expect(service.isMonitoring, isFalse);
      service.dispose();
    });

    test('stopMonitoring can be called multiple times', () async {
      final service = SensorDetectionService();
      await expectLater(service.stopMonitoring(), completes);
      await expectLater(service.stopMonitoring(), completes);
      service.dispose();
    });
  });

  group('SensorDetectionService AlarmSensitivity thresholds', () {
    test('sensitivity thresholds are properly defined', () {
      expect(AlarmSensitivity.low.accelerometerThreshold, greaterThan(0));
      expect(AlarmSensitivity.medium.accelerometerThreshold, greaterThan(0));
      expect(AlarmSensitivity.high.accelerometerThreshold, greaterThan(0));
      expect(AlarmSensitivity.veryHigh.accelerometerThreshold, greaterThan(0));

      expect(AlarmSensitivity.low.gyroscopeThreshold, greaterThan(0));
      expect(AlarmSensitivity.medium.gyroscopeThreshold, greaterThan(0));
      expect(AlarmSensitivity.high.gyroscopeThreshold, greaterThan(0));
      expect(AlarmSensitivity.veryHigh.gyroscopeThreshold, greaterThan(0));
    });

    test('sensitivity thresholds are in correct order', () {
      // Lower sensitivity = higher threshold (less sensitive)
      expect(AlarmSensitivity.low.accelerometerThreshold,
          greaterThan(AlarmSensitivity.medium.accelerometerThreshold));
      expect(AlarmSensitivity.medium.accelerometerThreshold,
          greaterThan(AlarmSensitivity.high.accelerometerThreshold));
      expect(AlarmSensitivity.high.accelerometerThreshold,
          greaterThan(AlarmSensitivity.veryHigh.accelerometerThreshold));
    });
  });
}
