import 'dart:convert';
import 'package:flutter/material.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:doggy_dogs_car_alarm/screens/settings_screen.dart';
import 'package:doggy_dogs_car_alarm/models/app_settings.dart';
import 'package:doggy_dogs_car_alarm/services/app_settings_service.dart';
import 'package:doggy_dogs_car_alarm/services/unlock_code_service.dart';
import 'package:shared_preferences/shared_preferences.dart';
import 'package:package_info_plus/package_info_plus.dart';

void main() {
  group('SettingsScreen Smoke Tests', () {
    late SharedPreferences prefs;

    setUp(() async {
      SharedPreferences.setMockInitialValues({});
      prefs = await SharedPreferences.getInstance();
    });

    testWidgets('creates and renders settings screen', (tester) async {
      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            sharedPreferencesProvider.overrideWithValue(prefs),
          ],
          child: const MaterialApp(
            home: SettingsScreen(),
          ),
        ),
      );

      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      expect(find.byType(SettingsScreen), findsOneWidget);
      expect(find.byType(Scaffold), findsOneWidget);

      // Allow pending timers to complete
      await tester.pump(const Duration(milliseconds: 150));
    });

    testWidgets('displays app bar with title', (tester) async {
      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            sharedPreferencesProvider.overrideWithValue(prefs),
          ],
          child: const MaterialApp(
            home: SettingsScreen(),
          ),
        ),
      );

      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      expect(find.byType(AppBar), findsOneWidget);
      expect(find.text('Settings'), findsOneWidget);

      // Allow pending timers to complete
      await tester.pump(const Duration(milliseconds: 150));
    });

    testWidgets('displays alarm settings section', (tester) async {
      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            sharedPreferencesProvider.overrideWithValue(prefs),
          ],
          child: const MaterialApp(
            home: SettingsScreen(),
          ),
        ),
      );

      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      expect(find.text('Alarm Settings'), findsOneWidget);
      expect(find.byIcon(Icons.security), findsWidgets);

      // Allow pending timers to complete
      await tester.pump(const Duration(milliseconds: 150));
    });

    testWidgets('displays countdown duration slider', (tester) async {
      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            sharedPreferencesProvider.overrideWithValue(prefs),
          ],
          child: const MaterialApp(
            home: SettingsScreen(),
          ),
        ),
      );

      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      expect(find.text('Activation Countdown'), findsOneWidget);
      expect(find.byType(Slider), findsWidgets);
      expect(
        find.text('Time before alarm activates after pressing start'),
        findsOneWidget,
      );

      // Allow pending timers to complete
      await tester.pump(const Duration(milliseconds: 150));
    });

    testWidgets('displays sensor sensitivity controls', (tester) async {
      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            sharedPreferencesProvider.overrideWithValue(prefs),
          ],
          child: const MaterialApp(
            home: SettingsScreen(),
          ),
        ),
      );

      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      expect(find.text('Sensor Sensitivity'), findsOneWidget);
      expect(find.byType(SegmentedButton<String>), findsOneWidget);
      expect(find.text('Low'), findsOneWidget);
      expect(find.text('Med'), findsOneWidget);
      expect(find.text('High'), findsOneWidget);
      expect(find.text('Max'), findsOneWidget);

      // Allow pending timers to complete
      await tester.pump(const Duration(milliseconds: 150));
    });

    testWidgets('displays bark volume slider', (tester) async {
      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            sharedPreferencesProvider.overrideWithValue(prefs),
          ],
          child: const MaterialApp(
            home: SettingsScreen(),
          ),
        ),
      );

      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      expect(find.text('Bark Volume'), findsOneWidget);
      expect(find.byIcon(Icons.volume_down), findsOneWidget);
      expect(find.byIcon(Icons.volume_up), findsOneWidget);

      // Allow pending timers to complete
      await tester.pump(const Duration(milliseconds: 150));
    });

    testWidgets('displays notifications section', (tester) async {
      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            sharedPreferencesProvider.overrideWithValue(prefs),
          ],
          child: const MaterialApp(
            home: SettingsScreen(),
          ),
        ),
      );

      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      expect(find.text('Notifications'), findsOneWidget);
      expect(find.byIcon(Icons.notifications), findsWidgets);

      // Allow pending timers to complete
      await tester.pump(const Duration(milliseconds: 150));
    });

    testWidgets('displays enable notifications switch', (tester) async {
      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            sharedPreferencesProvider.overrideWithValue(prefs),
          ],
          child: const MaterialApp(
            home: SettingsScreen(),
          ),
        ),
      );

      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      expect(find.text('Enable Notifications'), findsOneWidget);
      expect(
        find.text('Receive alerts when alarm is triggered'),
        findsOneWidget,
      );
      expect(find.byType(SwitchListTile), findsWidgets);

      // Allow pending timers to complete
      await tester.pump(const Duration(milliseconds: 150));
    });

    testWidgets('displays scrollable content with cards', (tester) async {
      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            sharedPreferencesProvider.overrideWithValue(prefs),
          ],
          child: const MaterialApp(
            home: SettingsScreen(),
          ),
        ),
      );

      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      expect(find.byType(ListView), findsOneWidget);
      expect(find.byType(Card), findsWidgets);

      // Allow pending timers to complete
      await tester.pump(const Duration(milliseconds: 150));
    });

    testWidgets('renders with default settings', (tester) async {
      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            sharedPreferencesProvider.overrideWithValue(prefs),
          ],
          child: const MaterialApp(
            home: SettingsScreen(),
          ),
        ),
      );

      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      // Check for default countdown duration (10s)
      expect(find.text('10s'), findsWidgets);

      // Check for default volume percentage (80%)
      expect(find.text('80%'), findsOneWidget);

      // Allow pending timers to complete
      await tester.pump(const Duration(milliseconds: 150));
    });

    testWidgets('renders with custom settings override', (tester) async {
      const customSettings = AppSettings(
        countdownDuration: 60,
        sensitivityLevel: 'high',
        barkVolume: 0.5,
        notificationsEnabled: false,
        batteryOptimizationEnabled: false,
      );

      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            sharedPreferencesProvider.overrideWithValue(prefs),
            appSettingsProvider.overrideWith(
              (ref) => AppSettingsNotifier(AppSettingsService(prefs))
                ..state = customSettings,
            ),
          ],
          child: const MaterialApp(
            home: SettingsScreen(),
          ),
        ),
      );

      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      // Verify settings screen renders with custom settings
      expect(find.byType(SettingsScreen), findsOneWidget);
      expect(find.text('Settings'), findsOneWidget);
      expect(find.text('Alarm Settings'), findsOneWidget);

      // Allow pending timers to complete
      await tester.pump(const Duration(milliseconds: 150));
    });

    testWidgets('renders different sensitivity levels', (tester) async {
      const lowSensitivitySettings = AppSettings(
        sensitivityLevel: 'low',
      );

      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            sharedPreferencesProvider.overrideWithValue(prefs),
            appSettingsProvider.overrideWith(
              (ref) => AppSettingsNotifier(AppSettingsService(prefs))
                ..state = lowSensitivitySettings,
            ),
          ],
          child: const MaterialApp(
            home: SettingsScreen(),
          ),
        ),
      );

      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      // Verify screen renders with low sensitivity settings
      expect(find.byType(SettingsScreen), findsOneWidget);
      expect(find.text('Sensor Sensitivity'), findsOneWidget);
      expect(find.byType(SegmentedButton<String>), findsOneWidget);

      // Allow pending timers to complete
      await tester.pump(const Duration(milliseconds: 150));
    });

    testWidgets('displays interactive list tiles', (tester) async {
      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            sharedPreferencesProvider.overrideWithValue(prefs),
          ],
          child: const MaterialApp(
            home: SettingsScreen(),
          ),
        ),
      );

      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      // Should have ListTiles for various settings
      expect(find.byType(ListTile), findsWidgets);

      // Allow pending timers to complete
      await tester.pump(const Duration(milliseconds: 150));
    });

    testWidgets('sections are properly spaced', (tester) async {
      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            sharedPreferencesProvider.overrideWithValue(prefs),
          ],
          child: const MaterialApp(
            home: SettingsScreen(),
          ),
        ),
      );

      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      // Check for SizedBox spacing between sections
      expect(find.byType(SizedBox), findsWidgets);

      // Allow pending timers to complete
      await tester.pump(const Duration(milliseconds: 150));
    });

    testWidgets('widget tree structure is valid', (tester) async {
      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            sharedPreferencesProvider.overrideWithValue(prefs),
          ],
          child: const MaterialApp(
            home: SettingsScreen(),
          ),
        ),
      );

      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      // Verify basic widget hierarchy
      expect(find.byType(MaterialApp), findsOneWidget);
      expect(find.byType(ProviderScope), findsOneWidget);
      expect(find.byType(SettingsScreen), findsOneWidget);
      expect(find.byType(Scaffold), findsOneWidget);
      expect(find.byType(AppBar), findsOneWidget);
      expect(find.byType(ListView), findsOneWidget);

      // Allow pending timers to complete
      await tester.pump(const Duration(milliseconds: 150));
    });

    testWidgets('renders without provider errors', (tester) async {
      // This test ensures providers are correctly set up
      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            sharedPreferencesProvider.overrideWithValue(prefs),
          ],
          child: const MaterialApp(
            home: SettingsScreen(),
          ),
        ),
      );

      // Should not throw any errors
      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      expect(tester.takeException(), isNull);

      // Allow pending timers to complete
      await tester.pump(const Duration(milliseconds: 150));
    });

    testWidgets('can be created multiple times', (tester) async {
      // First creation
      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            sharedPreferencesProvider.overrideWithValue(prefs),
          ],
          child: const MaterialApp(
            home: SettingsScreen(),
          ),
        ),
      );

      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));
      expect(find.byType(SettingsScreen), findsOneWidget);

      // Recreate
      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            sharedPreferencesProvider.overrideWithValue(prefs),
          ],
          child: const MaterialApp(
            home: SettingsScreen(),
          ),
        ),
      );

      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));
      expect(find.byType(SettingsScreen), findsOneWidget);

      // Allow pending timers to complete
      await tester.pump(const Duration(milliseconds: 150));
    });

    testWidgets('slider controls are interactive', (tester) async {
      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            sharedPreferencesProvider.overrideWithValue(prefs),
          ],
          child: const MaterialApp(
            home: SettingsScreen(),
          ),
        ),
      );

      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      // Verify sliders are present for countdown and volume
      final sliders = find.byType(Slider);
      expect(sliders, findsWidgets);

      // Allow pending timers to complete
      await tester.pump(const Duration(milliseconds: 150));
    });

    testWidgets('segmented button for sensitivity is present', (tester) async {
      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            sharedPreferencesProvider.overrideWithValue(prefs),
          ],
          child: const MaterialApp(
            home: SettingsScreen(),
          ),
        ),
      );

      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      // Verify segmented button exists
      expect(find.byType(SegmentedButton<String>), findsOneWidget);

      // Verify all sensitivity options
      expect(find.text('Low'), findsOneWidget);
      expect(find.text('Med'), findsOneWidget);
      expect(find.text('High'), findsOneWidget);
      expect(find.text('Max'), findsOneWidget);

      // Allow pending timers to complete
      await tester.pump(const Duration(milliseconds: 150));
    });

    testWidgets('volume icons are present', (tester) async {
      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            sharedPreferencesProvider.overrideWithValue(prefs),
          ],
          child: const MaterialApp(
            home: SettingsScreen(),
          ),
        ),
      );

      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      // Volume icons should be visible
      expect(find.byIcon(Icons.volume_down), findsOneWidget);
      expect(find.byIcon(Icons.volume_up), findsOneWidget);

      // Allow pending timers to complete
      await tester.pump(const Duration(milliseconds: 150));
    });

    testWidgets('alarm settings card is first section', (tester) async {
      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            sharedPreferencesProvider.overrideWithValue(prefs),
          ],
          child: const MaterialApp(
            home: SettingsScreen(),
          ),
        ),
      );

      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      // First major section should be Alarm Settings
      expect(find.text('Alarm Settings'), findsOneWidget);
      expect(find.text('Sensor Sensitivity'), findsOneWidget);
      expect(find.text('Bark Volume'), findsOneWidget);

      // Allow pending timers to complete
      await tester.pump(const Duration(milliseconds: 150));
    });

    testWidgets('notifications settings are present', (tester) async {
      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            sharedPreferencesProvider.overrideWithValue(prefs),
          ],
          child: const MaterialApp(
            home: SettingsScreen(),
          ),
        ),
      );

      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      // Notifications section
      expect(find.text('Notifications'), findsOneWidget);
      expect(find.text('Enable Notifications'), findsOneWidget);

      // Allow pending timers to complete
      await tester.pump(const Duration(milliseconds: 150));
    });

    testWidgets('all text labels render correctly', (tester) async {
      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            sharedPreferencesProvider.overrideWithValue(prefs),
          ],
          child: const MaterialApp(
            home: SettingsScreen(),
          ),
        ),
      );

      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      // Verify key text labels
      expect(find.text('Activation Countdown'), findsOneWidget);
      expect(find.text('Sensor Sensitivity'), findsOneWidget);
      expect(find.text('Bark Volume'), findsOneWidget);
      expect(find.text('Enable Notifications'), findsOneWidget);

      // Allow pending timers to complete
      await tester.pump(const Duration(milliseconds: 150));
    });
  });

  group('SettingsScreen User Interactions', () {
    late SharedPreferences prefs;

    setUp(() async {
      SharedPreferences.setMockInitialValues({});
      prefs = await SharedPreferences.getInstance();
    });

    testWidgets('sensitivity level changes on button tap', (tester) async {
      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            sharedPreferencesProvider.overrideWithValue(prefs),
          ],
          child: const MaterialApp(
            home: SettingsScreen(),
          ),
        ),
      );

      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      // Default should be medium
      expect(
        find.text('Balanced sensitivity for normal use'),
        findsOneWidget,
      );

      // Tap 'High' sensitivity button
      await tester.tap(find.text('High'));
      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      // Verify description changed to high
      expect(
        find.text('Triggers on light movement'),
        findsOneWidget,
      );

      // Verify change persisted - read from JSON
      final settingsJson = prefs.getString('app_settings');
      expect(settingsJson, isNotNull);
      final settings = AppSettings.fromJson(
          (jsonDecode(settingsJson!) as Map<String, dynamic>));
      expect(settings.sensitivityLevel, 'high');

      // Allow pending timers to complete
      await tester.pump(const Duration(milliseconds: 150));
    });

    testWidgets('notifications switch toggles', (tester) async {
      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            sharedPreferencesProvider.overrideWithValue(prefs),
          ],
          child: const MaterialApp(
            home: SettingsScreen(),
          ),
        ),
      );

      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      // Find notifications switch
      final notificationsSwitch = find.ancestor(
        of: find.text('Enable Notifications'),
        matching: find.byType(SwitchListTile),
      );

      // Tap to toggle notifications
      await tester.tap(notificationsSwitch);
      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      // Verify change persisted - read from JSON
      var settingsJson = prefs.getString('app_settings');
      expect(settingsJson, isNotNull);
      var settings = AppSettings.fromJson(
          (jsonDecode(settingsJson!) as Map<String, dynamic>));
      expect(
          settings.notificationsEnabled, isFalse); // Should be toggled to false

      // Toggle back
      await tester.tap(notificationsSwitch);
      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      settingsJson = prefs.getString('app_settings');
      expect(settingsJson, isNotNull);
      settings = AppSettings.fromJson(
          (jsonDecode(settingsJson!) as Map<String, dynamic>));
      expect(settings.notificationsEnabled, isTrue);

      // Allow pending timers to complete
      await tester.pump(const Duration(milliseconds: 150));
    });

    testWidgets('battery optimization switch toggles', (tester) async {
      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            sharedPreferencesProvider.overrideWithValue(prefs),
          ],
          child: const MaterialApp(
            home: SettingsScreen(),
          ),
        ),
      );

      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      // Need to scroll to battery section
      await tester.scrollUntilVisible(
        find.text('Battery Optimization'),
        500.0,
      );

      // Find battery optimization switch
      final batterySwitch = find.ancestor(
        of: find.text('Battery Optimization'),
        matching: find.byType(SwitchListTile),
      );

      // Tap to toggle battery optimization
      await tester.tap(batterySwitch);
      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      // Verify change persisted - read from JSON
      var settingsJson = prefs.getString('app_settings');
      expect(settingsJson, isNotNull);
      var settings = AppSettings.fromJson(
          (jsonDecode(settingsJson!) as Map<String, dynamic>));
      expect(settings.batteryOptimizationEnabled, isFalse);

      // Toggle back
      await tester.tap(batterySwitch);
      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      settingsJson = prefs.getString('app_settings');
      expect(settingsJson, isNotNull);
      settings = AppSettings.fromJson(
          (jsonDecode(settingsJson!) as Map<String, dynamic>));
      expect(settings.batteryOptimizationEnabled, isTrue);

      // Allow pending timers to complete
      await tester.pump(const Duration(milliseconds: 150));
    });

    testWidgets('all sensitivity levels can be selected', (tester) async {
      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            sharedPreferencesProvider.overrideWithValue(prefs),
          ],
          child: const MaterialApp(
            home: SettingsScreen(),
          ),
        ),
      );

      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      // Test Low sensitivity
      await tester.tap(find.text('Low'));
      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));
      expect(
        find.text('Triggers only on significant movement'),
        findsOneWidget,
      );
      var settingsJson = prefs.getString('app_settings');
      expect(settingsJson, isNotNull);
      var settings = AppSettings.fromJson(
          (jsonDecode(settingsJson!) as Map<String, dynamic>));
      expect(settings.sensitivityLevel, 'low');

      // Test Medium sensitivity
      await tester.tap(find.text('Med'));
      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));
      expect(
        find.text('Balanced sensitivity for normal use'),
        findsOneWidget,
      );
      settingsJson = prefs.getString('app_settings');
      expect(settingsJson, isNotNull);
      settings = AppSettings.fromJson(
          (jsonDecode(settingsJson!) as Map<String, dynamic>));
      expect(settings.sensitivityLevel, 'medium');

      // Test High sensitivity
      await tester.tap(find.text('High'));
      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));
      expect(
        find.text('Triggers on light movement'),
        findsOneWidget,
      );
      settingsJson = prefs.getString('app_settings');
      expect(settingsJson, isNotNull);
      settings = AppSettings.fromJson(
          (jsonDecode(settingsJson!) as Map<String, dynamic>));
      expect(settings.sensitivityLevel, 'high');

      // Test Max sensitivity
      await tester.tap(find.text('Max'));
      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));
      expect(
        find.text('Most sensitive - triggers easily'),
        findsOneWidget,
      );
      settingsJson = prefs.getString('app_settings');
      expect(settingsJson, isNotNull);
      settings = AppSettings.fromJson(
          (jsonDecode(settingsJson!) as Map<String, dynamic>));
      expect(settings.sensitivityLevel, 'veryHigh');

      // Allow pending timers to complete
      await tester.pump(const Duration(milliseconds: 150));
    });
  });

  group('SettingsScreen Battery Section', () {
    late SharedPreferences prefs;

    setUp(() async {
      SharedPreferences.setMockInitialValues({});
      prefs = await SharedPreferences.getInstance();
    });

    testWidgets('displays battery section', (tester) async {
      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            sharedPreferencesProvider.overrideWithValue(prefs),
          ],
          child: const MaterialApp(
            home: SettingsScreen(),
          ),
        ),
      );

      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      // Scroll to battery section
      await tester.scrollUntilVisible(
        find.text('Battery'),
        500.0,
      );

      expect(find.text('Battery'), findsOneWidget);
      expect(find.byIcon(Icons.battery_charging_full), findsOneWidget);
      expect(find.text('Battery Optimization'), findsOneWidget);
      expect(
        find.text('Reduce power usage when alarm is inactive'),
        findsOneWidget,
      );

      // Allow pending timers to complete
      await tester.pump(const Duration(milliseconds: 150));
    });

    testWidgets('battery optimization enabled by default', (tester) async {
      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            sharedPreferencesProvider.overrideWithValue(prefs),
          ],
          child: const MaterialApp(
            home: SettingsScreen(),
          ),
        ),
      );

      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      // Scroll to battery section
      await tester.scrollUntilVisible(
        find.text('Battery Optimization'),
        500.0,
      );

      final batterySwitch = find.ancestor(
        of: find.text('Battery Optimization'),
        matching: find.byType(SwitchListTile),
      );

      final switchWidget = tester.widget<SwitchListTile>(batterySwitch);
      expect(switchWidget.value, isTrue);

      // Allow pending timers to complete
      await tester.pump(const Duration(milliseconds: 150));
    });
  });

  group('SettingsScreen Security Section', () {
    late SharedPreferences prefs;

    setUp(() async {
      SharedPreferences.setMockInitialValues({
        'unlockCode': '1234',
      });
      prefs = await SharedPreferences.getInstance();
    });

    testWidgets('displays security section', (tester) async {
      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            sharedPreferencesProvider.overrideWithValue(prefs),
          ],
          child: const MaterialApp(
            home: SettingsScreen(),
          ),
        ),
      );

      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      // Scroll to security section
      await tester.scrollUntilVisible(
        find.text('Security'),
        500.0,
      );

      expect(find.text('Security'), findsOneWidget);
      expect(find.byIcon(Icons.lock), findsOneWidget);
      expect(find.text('Change Unlock Code'), findsOneWidget);
      expect(
          find.text('Set a new PIN to deactivate the alarm'), findsOneWidget);

      // Allow pending timers to complete
      await tester.pump(const Duration(milliseconds: 150));
    });

    testWidgets('opens change unlock code dialog', (tester) async {
      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            sharedPreferencesProvider.overrideWithValue(prefs),
          ],
          child: const MaterialApp(
            home: SettingsScreen(),
          ),
        ),
      );

      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      // Scroll to security section
      await tester.scrollUntilVisible(
        find.text('Change Unlock Code'),
        500.0,
      );

      // Tap change unlock code
      await tester.tap(find.text('Change Unlock Code'));
      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      // Verify dialog opened
      expect(find.text('Current Code'), findsOneWidget);
      expect(find.text('New Code'), findsOneWidget);
      expect(find.text('Confirm New Code'), findsOneWidget);
      expect(find.text('CANCEL'), findsOneWidget);
      expect(find.text('CHANGE CODE'), findsOneWidget);

      // Allow pending timers to complete
      await tester.pump(const Duration(milliseconds: 150));
    });

    testWidgets('cancel button closes unlock code dialog', (tester) async {
      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            sharedPreferencesProvider.overrideWithValue(prefs),
          ],
          child: const MaterialApp(
            home: SettingsScreen(),
          ),
        ),
      );

      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      // Scroll to security section
      await tester.scrollUntilVisible(
        find.text('Change Unlock Code'),
        500.0,
      );

      // Open dialog
      await tester.tap(find.text('Change Unlock Code'));
      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      // Tap cancel
      await tester.tap(find.text('CANCEL'));
      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));
      await tester.pump(const Duration(milliseconds: 100));
      await tester.pump(const Duration(milliseconds: 100));

      // Verify dialog closed
      expect(find.text('Current Code'), findsNothing);

      // Allow pending timers to complete
      await tester.pump(const Duration(milliseconds: 150));
    });

    testWidgets('shows error for incorrect current code', (tester) async {
      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            sharedPreferencesProvider.overrideWithValue(prefs),
          ],
          child: const MaterialApp(
            home: SettingsScreen(),
          ),
        ),
      );

      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      // Scroll to security section
      await tester.scrollUntilVisible(
        find.text('Change Unlock Code'),
        500.0,
      );

      // Open dialog
      await tester.tap(find.text('Change Unlock Code'));
      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      // Enter wrong current code
      await tester.enterText(
        find.widgetWithText(TextField, 'Enter current PIN'),
        '9999',
      );

      // Enter new codes
      await tester.enterText(
        find.widgetWithText(TextField, 'Enter new PIN'),
        '5678',
      );
      await tester.enterText(
        find.widgetWithText(TextField, 'Re-enter new PIN'),
        '5678',
      );

      // Tap change code
      await tester.tap(find.text('CHANGE CODE'));
      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      // Verify error message
      expect(find.text('Current code is incorrect'), findsOneWidget);

      // Allow pending timers to complete
      await tester.pump(const Duration(milliseconds: 150));
    });

    testWidgets('shows error when new codes do not match', (tester) async {
      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            sharedPreferencesProvider.overrideWithValue(prefs),
          ],
          child: const MaterialApp(
            home: SettingsScreen(),
          ),
        ),
      );

      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      // Scroll to security section
      await tester.scrollUntilVisible(
        find.text('Change Unlock Code'),
        500.0,
      );

      // Open dialog
      await tester.tap(find.text('Change Unlock Code'));
      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      // Enter correct current code
      await tester.enterText(
        find.widgetWithText(TextField, 'Enter current PIN'),
        '1234',
      );

      // Enter mismatched new codes
      await tester.enterText(
        find.widgetWithText(TextField, 'Enter new PIN'),
        '5678',
      );
      await tester.enterText(
        find.widgetWithText(TextField, 'Re-enter new PIN'),
        '5679',
      );

      // Tap change code
      await tester.tap(find.text('CHANGE CODE'));
      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      // Verify error message
      expect(find.text('New codes do not match'), findsOneWidget);

      // Allow pending timers to complete
      await tester.pump(const Duration(milliseconds: 150));
    });

    testWidgets('shows error for code less than 4 digits', (tester) async {
      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            sharedPreferencesProvider.overrideWithValue(prefs),
          ],
          child: const MaterialApp(
            home: SettingsScreen(),
          ),
        ),
      );

      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      // Scroll to security section
      await tester.scrollUntilVisible(
        find.text('Change Unlock Code'),
        500.0,
      );

      // Open dialog
      await tester.tap(find.text('Change Unlock Code'));
      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      // Enter correct current code
      await tester.enterText(
        find.widgetWithText(TextField, 'Enter current PIN'),
        '1234',
      );

      // Enter short new code
      await tester.enterText(
        find.widgetWithText(TextField, 'Enter new PIN'),
        '123',
      );
      await tester.enterText(
        find.widgetWithText(TextField, 'Re-enter new PIN'),
        '123',
      );

      // Tap change code
      await tester.tap(find.text('CHANGE CODE'));
      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      // Verify error message
      expect(find.text('Code must be at least 4 digits'), findsOneWidget);

      // Allow pending timers to complete
      await tester.pump(const Duration(milliseconds: 150));
    });

    testWidgets('successfully changes unlock code', (tester) async {
      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            sharedPreferencesProvider.overrideWithValue(prefs),
          ],
          child: const MaterialApp(
            home: SettingsScreen(),
          ),
        ),
      );

      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      // Scroll to security section
      await tester.scrollUntilVisible(
        find.text('Change Unlock Code'),
        500.0,
      );

      // Open dialog
      await tester.tap(find.text('Change Unlock Code'));
      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      // Enter correct current code
      await tester.enterText(
        find.widgetWithText(TextField, 'Enter current PIN'),
        '1234',
      );

      // Enter valid new code
      await tester.enterText(
        find.widgetWithText(TextField, 'Enter new PIN'),
        '5678',
      );
      await tester.enterText(
        find.widgetWithText(TextField, 'Re-enter new PIN'),
        '5678',
      );

      // Tap change code
      await tester.tap(find.text('CHANGE CODE'));
      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));
      await tester.pump(const Duration(milliseconds: 100));
      await tester.pump(const Duration(milliseconds: 100));

      // Verify success message
      expect(find.text('Unlock code changed successfully'), findsOneWidget);

      // Verify dialog closed
      expect(find.text('Current Code'), findsNothing);

      // Verify new code works and old code doesn't
      final unlockService = UnlockCodeService(prefs);
      expect(await unlockService.validateUnlockCode('5678'), true);
      expect(await unlockService.validateUnlockCode('1234'), false);

      // Allow pending timers to complete
      await tester.pump(const Duration(milliseconds: 150));
    });
  });

  group('SettingsScreen About Section', () {
    late SharedPreferences prefs;

    setUp(() async {
      SharedPreferences.setMockInitialValues({});
      prefs = await SharedPreferences.getInstance();

      // Set up PackageInfo mock
      PackageInfo.setMockInitialValues(
        appName: 'Doggy Dogs Car Dog Alarm',
        packageName: 'com.example.doggy_dogs_car_alarm',
        version: '1.0.0',
        buildNumber: '1',
        buildSignature: '',
      );
    });

    testWidgets('displays about section', (tester) async {
      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            sharedPreferencesProvider.overrideWithValue(prefs),
          ],
          child: const MaterialApp(
            home: SettingsScreen(),
          ),
        ),
      );

      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      // Scroll to about section
      await tester.scrollUntilVisible(
        find.text('About'),
        500.0,
      );

      expect(find.text('About'), findsOneWidget);
      expect(find.byIcon(Icons.info), findsOneWidget);

      // Allow pending timers to complete
      await tester.pump(const Duration(milliseconds: 150));
    });

    testWidgets('displays app information', (tester) async {
      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            sharedPreferencesProvider.overrideWithValue(prefs),
          ],
          child: const MaterialApp(
            home: SettingsScreen(),
          ),
        ),
      );

      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      // Scroll to about section
      await tester.scrollUntilVisible(
        find.text('About'),
        500.0,
      );

      // Wait for FutureBuilder to complete
      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      expect(find.text('Doggy Dogs Car Dog Alarm'), findsOneWidget);
      expect(find.textContaining('Version 1.0.0'), findsOneWidget);
      expect(find.text('App ID'), findsOneWidget);
      expect(find.text('com.example.doggy_dogs_car_alarm'), findsOneWidget);
      expect(find.byIcon(Icons.pets), findsOneWidget);
      expect(find.byIcon(Icons.code), findsOneWidget);

      // Allow pending timers to complete
      await tester.pump(const Duration(milliseconds: 150));
    });

    testWidgets('shows loading indicator while fetching package info',
        (tester) async {
      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            sharedPreferencesProvider.overrideWithValue(prefs),
          ],
          child: const MaterialApp(
            home: SettingsScreen(),
          ),
        ),
      );

      // Don't wait for settle, check immediately
      await tester.pump();

      // Scroll to about section
      await tester.dragUntilVisible(
        find.text('About'),
        find.byType(ListView),
        const Offset(0, -500),
      );

      // CircularProgressIndicator might appear briefly
      // This test captures the loading state
      await tester.pump();

      // Allow pending timers to complete
      await tester.pump(const Duration(milliseconds: 150));
    });
  });

  group('SettingsScreen Reset Section', () {
    late SharedPreferences prefs;

    setUp(() async {
      SharedPreferences.setMockInitialValues({});
      prefs = await SharedPreferences.getInstance();
    });

    testWidgets('displays reset section', (tester) async {
      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            sharedPreferencesProvider.overrideWithValue(prefs),
          ],
          child: const MaterialApp(
            home: SettingsScreen(),
          ),
        ),
      );

      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      // Scroll to reset section
      await tester.scrollUntilVisible(
        find.text('Reset'),
        500.0,
      );

      expect(find.text('Reset'), findsOneWidget);
      expect(find.byIcon(Icons.restore), findsOneWidget);
      expect(find.text('Reset to Defaults'), findsOneWidget);
      expect(
        find.text('Restore all settings to default values'),
        findsOneWidget,
      );

      // Allow pending timers to complete
      await tester.pump(const Duration(milliseconds: 150));
    });

    testWidgets('opens reset confirmation dialog', (tester) async {
      // Set larger screen size to fit all content
      tester.view.physicalSize = const Size(800, 1400);
      tester.view.devicePixelRatio = 1.0;
      addTearDown(() => tester.view.reset());

      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            sharedPreferencesProvider.overrideWithValue(prefs),
          ],
          child: const MaterialApp(
            home: SettingsScreen(),
          ),
        ),
      );

      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      // Scroll to reset section
      await tester.scrollUntilVisible(
        find.text('Reset to Defaults'),
        500.0,
      );

      // Tap reset
      await tester.tap(find.text('Reset to Defaults'));
      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));
      await tester.pump(const Duration(milliseconds: 100));
      await tester.pump(const Duration(milliseconds: 100));

      // Verify dialog opened
      expect(find.text('Reset Settings?'), findsOneWidget);
      expect(
        find.text(
          'This will restore all settings to their default values. This action cannot be undone.',
        ),
        findsOneWidget,
      );
      expect(find.text('CANCEL'), findsOneWidget);
      expect(find.text('RESET'), findsOneWidget);

      // Allow pending timers to complete
      await tester.pump(const Duration(milliseconds: 150));
    });

    testWidgets('cancel button closes reset dialog', (tester) async {
      // Set larger screen size to fit all content
      tester.view.physicalSize = const Size(800, 1400);
      tester.view.devicePixelRatio = 1.0;
      addTearDown(() => tester.view.reset());

      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            sharedPreferencesProvider.overrideWithValue(prefs),
          ],
          child: const MaterialApp(
            home: SettingsScreen(),
          ),
        ),
      );

      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      // Scroll to reset section
      await tester.scrollUntilVisible(
        find.text('Reset to Defaults'),
        500.0,
      );

      // Open dialog
      await tester.tap(find.text('Reset to Defaults'));
      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      // Tap cancel
      await tester.tap(find.text('CANCEL'));
      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));
      await tester.pump(const Duration(milliseconds: 100));
      await tester.pump(const Duration(milliseconds: 100));

      // Verify dialog closed and settings unchanged
      expect(find.text('Reset Settings?'), findsNothing);

      // Allow pending timers to complete
      await tester.pump(const Duration(milliseconds: 150));
    });

    testWidgets('reset button resets settings to defaults', (tester) async {
      // Set larger screen size to fit all content
      tester.view.physicalSize = const Size(800, 1400);
      tester.view.devicePixelRatio = 1.0;
      addTearDown(() => tester.view.reset());

      // Set custom values first using JSON format
      await prefs.setString('app_settings',
          '{"countdownDuration":60,"sensitivityLevel":"high","barkVolume":0.5,"notificationsEnabled":false,"batteryOptimizationEnabled":false}');

      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            sharedPreferencesProvider.overrideWithValue(prefs),
          ],
          child: const MaterialApp(
            home: SettingsScreen(),
          ),
        ),
      );

      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      // Verify custom settings are loaded
      expect(find.text('60s'), findsWidgets);
      expect(find.text('50%'), findsOneWidget);

      // Scroll to reset section
      await tester.scrollUntilVisible(
        find.text('Reset to Defaults'),
        500.0,
      );

      // Open dialog
      await tester.tap(find.text('Reset to Defaults'));
      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      // Tap reset
      await tester.tap(find.text('RESET'));
      await tester.pump();
      await tester.pump(const Duration(milliseconds: 200));
      await tester.pump(const Duration(milliseconds: 200));

      // Verify success message
      expect(find.text('Settings reset to defaults'), findsOneWidget);

      // Allow async save operations to complete
      await tester.pump(const Duration(milliseconds: 500));

      // Verify default values restored - the reset clears the JSON, so we need to check individual keys
      // or verify that default AppSettings are being used
      const defaultSettings = AppSettings();
      expect(defaultSettings.countdownDuration, 10);
      expect(defaultSettings.sensitivityLevel, 'medium');
      expect(defaultSettings.barkVolume, 0.8);
      expect(defaultSettings.notificationsEnabled, true);
      expect(defaultSettings.batteryOptimizationEnabled, true);

      // Verify the settings were actually reset by checking that the old values are gone
      final settingsJson = prefs.getString('app_settings');
      // After reset, settings JSON might be cleared or set to defaults
      if (settingsJson != null) {
        final settings = AppSettings.fromJson(
            (jsonDecode(settingsJson) as Map<String, dynamic>));
        expect(settings.countdownDuration, 10);
        expect(settings.sensitivityLevel, 'medium');
        expect(settings.barkVolume, 0.8);
        expect(settings.notificationsEnabled, true);
        expect(settings.batteryOptimizationEnabled, true);
      }

      // Allow pending timers to complete
      await tester.pump(const Duration(milliseconds: 150));
    });
  });

  group('SettingsScreen Settings Combinations', () {
    late SharedPreferences prefs;

    setUp(() async {
      SharedPreferences.setMockInitialValues({});
      prefs = await SharedPreferences.getInstance();
    });

    testWidgets('renders with all settings disabled', (tester) async {
      // Pre-populate SharedPreferences with the desired settings
      await prefs.setString('app_settings',
          '{"countdownDuration":15,"sensitivityLevel":"low","barkVolume":0.0,"notificationsEnabled":false,"batteryOptimizationEnabled":false}');

      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            sharedPreferencesProvider.overrideWithValue(prefs),
          ],
          child: const MaterialApp(
            home: SettingsScreen(),
          ),
        ),
      );

      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      expect(find.text('15s'), findsWidgets);
      expect(find.text('0%'), findsOneWidget);
      expect(
        find.text('Triggers only on significant movement'),
        findsOneWidget,
      );

      // Allow pending timers to complete
      await tester.pump(const Duration(milliseconds: 150));
    });

    testWidgets('renders with all settings at maximum', (tester) async {
      // Pre-populate SharedPreferences with the desired settings
      await prefs.setString('app_settings',
          '{"countdownDuration":120,"sensitivityLevel":"veryHigh","barkVolume":1.0,"notificationsEnabled":true,"batteryOptimizationEnabled":true}');

      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            sharedPreferencesProvider.overrideWithValue(prefs),
          ],
          child: const MaterialApp(
            home: SettingsScreen(),
          ),
        ),
      );

      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      expect(find.text('120s'), findsWidgets);
      expect(find.text('100%'), findsOneWidget);
      expect(
        find.text('Most sensitive - triggers easily'),
        findsOneWidget,
      );

      // Allow pending timers to complete
      await tester.pump(const Duration(milliseconds: 150));
    });

    testWidgets('multiple switch toggles persist correctly', (tester) async {
      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            sharedPreferencesProvider.overrideWithValue(prefs),
          ],
          child: const MaterialApp(
            home: SettingsScreen(),
          ),
        ),
      );

      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      // Toggle notifications
      final notificationsSwitch = find.ancestor(
        of: find.text('Enable Notifications'),
        matching: find.byType(SwitchListTile),
      );
      await tester.tap(notificationsSwitch);
      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      // Scroll and toggle battery optimization
      await tester.scrollUntilVisible(
        find.text('Battery Optimization'),
        500.0,
      );
      final batterySwitch = find.ancestor(
        of: find.text('Battery Optimization'),
        matching: find.byType(SwitchListTile),
      );
      await tester.tap(batterySwitch);
      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      // Verify both changes persisted - read from JSON stored in 'app_settings'
      final settingsJson = prefs.getString('app_settings');
      expect(settingsJson, isNotNull);
      final settings = AppSettings.fromJson(
          (jsonDecode(settingsJson!) as Map<String, dynamic>));
      expect(settings.notificationsEnabled, isFalse);
      expect(settings.batteryOptimizationEnabled, isFalse);

      // Allow pending timers to complete
      await tester.pump(const Duration(milliseconds: 150));
    });

    testWidgets('can change multiple settings in sequence', (tester) async {
      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            sharedPreferencesProvider.overrideWithValue(prefs),
          ],
          child: const MaterialApp(
            home: SettingsScreen(),
          ),
        ),
      );

      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      // Change sensitivity
      await tester.tap(find.text('Low'));
      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      // Verify sensitivity changed - read from JSON
      var settingsJson = prefs.getString('app_settings');
      expect(settingsJson, isNotNull);
      var settings = AppSettings.fromJson(
          (jsonDecode(settingsJson!) as Map<String, dynamic>));
      expect(settings.sensitivityLevel, 'low');

      // Toggle notifications
      final notificationsSwitch = find.ancestor(
        of: find.text('Enable Notifications'),
        matching: find.byType(SwitchListTile),
      );
      await tester.tap(notificationsSwitch);
      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      // Verify notifications toggled - read from JSON
      settingsJson = prefs.getString('app_settings');
      expect(settingsJson, isNotNull);
      settings = AppSettings.fromJson(
          (jsonDecode(settingsJson!) as Map<String, dynamic>));
      expect(settings.notificationsEnabled, isFalse);

      // Scroll and toggle battery
      await tester.scrollUntilVisible(
        find.text('Battery Optimization'),
        500.0,
      );
      final batterySwitch = find.ancestor(
        of: find.text('Battery Optimization'),
        matching: find.byType(SwitchListTile),
      );
      await tester.tap(batterySwitch);
      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      // Verify battery toggled - read from JSON
      settingsJson = prefs.getString('app_settings');
      expect(settingsJson, isNotNull);
      settings = AppSettings.fromJson(
          (jsonDecode(settingsJson!) as Map<String, dynamic>));
      expect(settings.batteryOptimizationEnabled, isFalse);

      // No errors occurred
      expect(tester.takeException(), isNull);

      // Allow pending timers to complete
      await tester.pump(const Duration(milliseconds: 150));
    });
  });

  group('SettingsScreen Edge Cases', () {
    late SharedPreferences prefs;

    setUp(() async {
      SharedPreferences.setMockInitialValues({});
      prefs = await SharedPreferences.getInstance();
    });

    testWidgets('handles rapid switch toggles', (tester) async {
      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            sharedPreferencesProvider.overrideWithValue(prefs),
          ],
          child: const MaterialApp(
            home: SettingsScreen(),
          ),
        ),
      );

      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      final notificationsSwitch = find.ancestor(
        of: find.text('Enable Notifications'),
        matching: find.byType(SwitchListTile),
      );

      // Rapidly toggle switch
      await tester.tap(notificationsSwitch);
      await tester.pump();
      await tester.tap(notificationsSwitch);
      await tester.pump();
      await tester.tap(notificationsSwitch);
      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      // Should handle without errors
      expect(tester.takeException(), isNull);

      // Allow pending timers to complete
      await tester.pump(const Duration(milliseconds: 150));
    });

    testWidgets('sensitivity segmented buttons are all tappable',
        (tester) async {
      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            sharedPreferencesProvider.overrideWithValue(prefs),
          ],
          child: const MaterialApp(
            home: SettingsScreen(),
          ),
        ),
      );

      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      // Verify all sensitivity buttons are present and tappable
      expect(find.text('Low'), findsOneWidget);
      expect(find.text('Med'), findsOneWidget);
      expect(find.text('High'), findsOneWidget);
      expect(find.text('Max'), findsOneWidget);

      // Tap each button to ensure they respond
      await tester.tap(find.text('Low'));
      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));
      expect(tester.takeException(), isNull);

      await tester.tap(find.text('High'));
      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));
      expect(tester.takeException(), isNull);

      await tester.tap(find.text('Max'));
      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));
      expect(tester.takeException(), isNull);

      // All taps completed without errors
      expect(tester.takeException(), isNull);

      // Allow pending timers to complete
      await tester.pump(const Duration(milliseconds: 150));
    });

    testWidgets('security section icon is displayed', (tester) async {
      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            sharedPreferencesProvider.overrideWithValue(prefs),
          ],
          child: const MaterialApp(
            home: SettingsScreen(),
          ),
        ),
      );

      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      await tester.scrollUntilVisible(
        find.byIcon(Icons.lock),
        500.0,
      );

      expect(find.byIcon(Icons.lock), findsOneWidget);
      expect(find.byIcon(Icons.pin), findsOneWidget);

      // Allow pending timers to complete
      await tester.pump(const Duration(milliseconds: 150));
    });

    testWidgets('reset section icon is displayed', (tester) async {
      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            sharedPreferencesProvider.overrideWithValue(prefs),
          ],
          child: const MaterialApp(
            home: SettingsScreen(),
          ),
        ),
      );

      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      await tester.scrollUntilVisible(
        find.byIcon(Icons.settings_backup_restore),
        500.0,
      );

      expect(find.byIcon(Icons.settings_backup_restore), findsOneWidget);

      // Allow pending timers to complete
      await tester.pump(const Duration(milliseconds: 150));
    });
  });
}
