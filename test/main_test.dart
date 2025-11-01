import 'package:flutter/material.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:doggy_dogs_car_alarm/main.dart';
import 'package:doggy_dogs_car_alarm/models/dog.dart';
import 'package:doggy_dogs_car_alarm/providers/dog_provider.dart';
import 'package:doggy_dogs_car_alarm/screens/home_screen.dart';
import 'package:doggy_dogs_car_alarm/screens/breed_selection_screen.dart';
import 'package:shared_preferences/shared_preferences.dart';

void main() {
  group('DoggyDogsCarAlarmApp Tests', () {
    setUp(() async {
      SharedPreferences.setMockInitialValues({});
    });

    testWidgets('app creates MaterialApp with correct title', (tester) async {
      final testDog = Dog(
        id: 'test',
        name: 'Rex',
        breed: DogBreed.germanShepherd,
        stats:
            const DogStats(hunger: 80, happiness: 80, energy: 80, loyalty: 80),
        personality: const DogPersonality(protective: true, brave: true),
        createdAt: DateTime.now(),
        lastInteraction: DateTime.now(),
      );

      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            dogProvider.overrideWith((ref) => DogNotifier()..state = testDog),
          ],
          child: const DoggyDogsCarAlarmApp(),
        ),
      );

      await tester.pumpAndSettle();

      // MaterialApp should be created
      expect(find.byType(MaterialApp), findsOneWidget);
    });

    testWidgets('app has correct theme configuration', (tester) async {
      final testDog = Dog(
        id: 'test',
        name: 'Rex',
        breed: DogBreed.germanShepherd,
        stats:
            const DogStats(hunger: 80, happiness: 80, energy: 80, loyalty: 80),
        personality: const DogPersonality(protective: true, brave: true),
        createdAt: DateTime.now(),
        lastInteraction: DateTime.now(),
      );

      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            dogProvider.overrideWith((ref) => DogNotifier()..state = testDog),
          ],
          child: const DoggyDogsCarAlarmApp(),
        ),
      );

      await tester.pumpAndSettle();

      final materialApp = tester.widget<MaterialApp>(find.byType(MaterialApp));

      // Verify theme properties
      expect(materialApp.theme, isNotNull);
      expect(materialApp.theme!.useMaterial3, true);
      expect(materialApp.debugShowCheckedModeBanner, false);
      expect(materialApp.title, 'Doggy Dogs Car Alarm');
    });

    testWidgets('app uses Material 3', (tester) async {
      final testDog = Dog(
        id: 'test',
        name: 'Rex',
        breed: DogBreed.germanShepherd,
        stats:
            const DogStats(hunger: 80, happiness: 80, energy: 80, loyalty: 80),
        personality: const DogPersonality(protective: true, brave: true),
        createdAt: DateTime.now(),
        lastInteraction: DateTime.now(),
      );

      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            dogProvider.overrideWith((ref) => DogNotifier()..state = testDog),
          ],
          child: const DoggyDogsCarAlarmApp(),
        ),
      );

      await tester.pumpAndSettle();

      final materialApp = tester.widget<MaterialApp>(find.byType(MaterialApp));

      expect(materialApp.theme!.useMaterial3, true);
      expect(materialApp.theme!.colorScheme.primary, isNotNull);
    });

    testWidgets('app home is AppStartupScreen', (tester) async {
      final testDog = Dog(
        id: 'test',
        name: 'Rex',
        breed: DogBreed.germanShepherd,
        stats:
            const DogStats(hunger: 80, happiness: 80, energy: 80, loyalty: 80),
        personality: const DogPersonality(protective: true, brave: true),
        createdAt: DateTime.now(),
        lastInteraction: DateTime.now(),
      );

      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            dogProvider.overrideWith((ref) => DogNotifier()..state = testDog),
          ],
          child: const DoggyDogsCarAlarmApp(),
        ),
      );

      await tester.pumpAndSettle();

      final materialApp = tester.widget<MaterialApp>(find.byType(MaterialApp));
      expect(materialApp.home, isA<AppStartupScreen>());
    });

    testWidgets('app has card theme with rounded corners', (tester) async {
      final testDog = Dog(
        id: 'test',
        name: 'Rex',
        breed: DogBreed.germanShepherd,
        stats:
            const DogStats(hunger: 80, happiness: 80, energy: 80, loyalty: 80),
        personality: const DogPersonality(protective: true, brave: true),
        createdAt: DateTime.now(),
        lastInteraction: DateTime.now(),
      );

      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            dogProvider.overrideWith((ref) => DogNotifier()..state = testDog),
          ],
          child: const DoggyDogsCarAlarmApp(),
        ),
      );

      await tester.pumpAndSettle();

      final materialApp = tester.widget<MaterialApp>(find.byType(MaterialApp));

      expect(materialApp.theme!.cardTheme.elevation, 4);
      expect(materialApp.theme!.cardTheme.shape, isA<RoundedRectangleBorder>());
    });

    testWidgets('app has elevated button theme', (tester) async {
      final testDog = Dog(
        id: 'test',
        name: 'Rex',
        breed: DogBreed.germanShepherd,
        stats:
            const DogStats(hunger: 80, happiness: 80, energy: 80, loyalty: 80),
        personality: const DogPersonality(protective: true, brave: true),
        createdAt: DateTime.now(),
        lastInteraction: DateTime.now(),
      );

      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            dogProvider.overrideWith((ref) => DogNotifier()..state = testDog),
          ],
          child: const DoggyDogsCarAlarmApp(),
        ),
      );

      await tester.pumpAndSettle();

      final materialApp = tester.widget<MaterialApp>(find.byType(MaterialApp));

      expect(materialApp.theme!.elevatedButtonTheme, isNotNull);
      expect(materialApp.theme!.elevatedButtonTheme.style, isNotNull);
    });
  });

  group('AppStartupScreen Tests', () {
    setUp(() async {
      SharedPreferences.setMockInitialValues({});
    });

    testWidgets('shows loading indicator initially when no dog',
        (tester) async {
      await tester.pumpWidget(
        const ProviderScope(
          child: MaterialApp(
            home: AppStartupScreen(),
          ),
        ),
      );

      // Should show loading indicator initially
      expect(find.byType(CircularProgressIndicator), findsOneWidget);

      // Clean up timer before test ends
      await tester.pump(const Duration(milliseconds: 500));
      await tester.pumpAndSettle();
    });

    testWidgets('shows breed selection screen after loading when no dog',
        (tester) async {
      await tester.pumpWidget(
        const ProviderScope(
          child: MaterialApp(
            home: AppStartupScreen(),
          ),
        ),
      );

      // Wait for initial loading
      await tester.pump(const Duration(milliseconds: 500));
      await tester.pumpAndSettle();

      // Should show breed selection screen (onboarding)
      expect(find.byType(BreedSelectionScreen), findsOneWidget);
    });

    testWidgets('shows home screen when dog exists', (tester) async {
      final testDog = Dog(
        id: 'test',
        name: 'Rex',
        breed: DogBreed.germanShepherd,
        stats:
            const DogStats(hunger: 80, happiness: 80, energy: 80, loyalty: 80),
        personality: const DogPersonality(protective: true, brave: true),
        createdAt: DateTime.now(),
        lastInteraction: DateTime.now(),
      );

      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            dogProvider.overrideWith((ref) => DogNotifier()..state = testDog),
          ],
          child: const MaterialApp(
            home: AppStartupScreen(),
          ),
        ),
      );

      await tester.pumpAndSettle();

      // Should show home screen directly
      expect(find.byType(HomeScreen), findsOneWidget);
    });

    testWidgets('AppStartupScreen is a ConsumerWidget', (tester) async {
      await tester.pumpWidget(
        const ProviderScope(
          child: MaterialApp(
            home: AppStartupScreen(),
          ),
        ),
      );

      expect(find.byType(AppStartupScreen), findsOneWidget);

      final widget = tester.widget(find.byType(AppStartupScreen));
      expect(widget, isA<ConsumerWidget>());

      // Clean up timer before test ends
      await tester.pump(const Duration(milliseconds: 500));
      await tester.pumpAndSettle();
    });

    testWidgets('shows scaffold with loading indicator while waiting',
        (tester) async {
      await tester.pumpWidget(
        const ProviderScope(
          child: MaterialApp(
            home: AppStartupScreen(),
          ),
        ),
      );

      // Should show Scaffold with CircularProgressIndicator
      expect(find.byType(Scaffold), findsOneWidget);
      expect(find.byType(CircularProgressIndicator), findsOneWidget);
      expect(find.byType(Center), findsWidgets);

      // Clean up timer before test ends
      await tester.pump(const Duration(milliseconds: 500));
      await tester.pumpAndSettle();
    });

    testWidgets('BreedSelectionScreen has isOnboarding=true when no dog',
        (tester) async {
      await tester.pumpWidget(
        const ProviderScope(
          child: MaterialApp(
            home: AppStartupScreen(),
          ),
        ),
      );

      await tester.pump(const Duration(milliseconds: 500));
      await tester.pumpAndSettle();

      final breedScreen = tester.widget<BreedSelectionScreen>(
        find.byType(BreedSelectionScreen),
      );

      expect(breedScreen.isOnboarding, true);
    });
  });

  group('DoggyDogsCarAlarmApp Widget Structure Tests', () {
    setUp(() async {
      SharedPreferences.setMockInitialValues({});
    });

    testWidgets('DoggyDogsCarAlarmApp is a ConsumerWidget', (tester) async {
      final testDog = Dog(
        id: 'test',
        name: 'Rex',
        breed: DogBreed.germanShepherd,
        stats:
            const DogStats(hunger: 80, happiness: 80, energy: 80, loyalty: 80),
        personality: const DogPersonality(protective: true, brave: true),
        createdAt: DateTime.now(),
        lastInteraction: DateTime.now(),
      );

      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            dogProvider.overrideWith((ref) => DogNotifier()..state = testDog),
          ],
          child: const DoggyDogsCarAlarmApp(),
        ),
      );

      await tester.pumpAndSettle();

      final widget = tester.widget(find.byType(DoggyDogsCarAlarmApp));
      expect(widget, isA<ConsumerWidget>());
    });

    testWidgets('app builds without errors with dog', (tester) async {
      final testDog = Dog(
        id: 'test',
        name: 'Rex',
        breed: DogBreed.germanShepherd,
        stats:
            const DogStats(hunger: 80, happiness: 80, energy: 80, loyalty: 80),
        personality: const DogPersonality(protective: true, brave: true),
        createdAt: DateTime.now(),
        lastInteraction: DateTime.now(),
      );

      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            dogProvider.overrideWith((ref) => DogNotifier()..state = testDog),
          ],
          child: const DoggyDogsCarAlarmApp(),
        ),
      );

      // Should complete without throwing
      await tester.pumpAndSettle();

      expect(find.byType(MaterialApp), findsOneWidget);
    });

    testWidgets('app has consistent key', (tester) async {
      const key = Key('test_app');

      final testDog = Dog(
        id: 'test',
        name: 'Rex',
        breed: DogBreed.germanShepherd,
        stats:
            const DogStats(hunger: 80, happiness: 80, energy: 80, loyalty: 80),
        personality: const DogPersonality(protective: true, brave: true),
        createdAt: DateTime.now(),
        lastInteraction: DateTime.now(),
      );

      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            dogProvider.overrideWith((ref) => DogNotifier()..state = testDog),
          ],
          child: const DoggyDogsCarAlarmApp(key: key),
        ),
      );

      await tester.pumpAndSettle();

      expect(find.byKey(key), findsOneWidget);
    });
  });

  group('Theme Configuration Tests', () {
    setUp(() async {
      SharedPreferences.setMockInitialValues({});
    });

    testWidgets('theme has light color scheme', (tester) async {
      final testDog = Dog(
        id: 'test',
        name: 'Rex',
        breed: DogBreed.germanShepherd,
        stats:
            const DogStats(hunger: 80, happiness: 80, energy: 80, loyalty: 80),
        personality: const DogPersonality(protective: true, brave: true),
        createdAt: DateTime.now(),
        lastInteraction: DateTime.now(),
      );

      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            dogProvider.overrideWith((ref) => DogNotifier()..state = testDog),
          ],
          child: const DoggyDogsCarAlarmApp(),
        ),
      );

      await tester.pumpAndSettle();

      final materialApp = tester.widget<MaterialApp>(find.byType(MaterialApp));
      final colorScheme = materialApp.theme!.colorScheme;

      // Verify it's a light theme
      expect(colorScheme.brightness, Brightness.light);
    });

    testWidgets('theme has text theme configured', (tester) async {
      final testDog = Dog(
        id: 'test',
        name: 'Rex',
        breed: DogBreed.germanShepherd,
        stats:
            const DogStats(hunger: 80, happiness: 80, energy: 80, loyalty: 80),
        personality: const DogPersonality(protective: true, brave: true),
        createdAt: DateTime.now(),
        lastInteraction: DateTime.now(),
      );

      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            dogProvider.overrideWith((ref) => DogNotifier()..state = testDog),
          ],
          child: const DoggyDogsCarAlarmApp(),
        ),
      );

      await tester.pumpAndSettle();

      final materialApp = tester.widget<MaterialApp>(find.byType(MaterialApp));

      // Verify theme has text theme configured
      expect(materialApp.theme!.textTheme, isNotNull);
    });

    testWidgets('card theme has border radius of 16', (tester) async {
      final testDog = Dog(
        id: 'test',
        name: 'Rex',
        breed: DogBreed.germanShepherd,
        stats:
            const DogStats(hunger: 80, happiness: 80, energy: 80, loyalty: 80),
        personality: const DogPersonality(protective: true, brave: true),
        createdAt: DateTime.now(),
        lastInteraction: DateTime.now(),
      );

      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            dogProvider.overrideWith((ref) => DogNotifier()..state = testDog),
          ],
          child: const DoggyDogsCarAlarmApp(),
        ),
      );

      await tester.pumpAndSettle();

      final materialApp = tester.widget<MaterialApp>(find.byType(MaterialApp));
      final cardShape =
          materialApp.theme!.cardTheme.shape as RoundedRectangleBorder;
      final borderRadius = cardShape.borderRadius as BorderRadius;

      expect(borderRadius.topLeft.x, 16);
    });

    testWidgets('elevated button theme has proper configuration',
        (tester) async {
      final testDog = Dog(
        id: 'test',
        name: 'Rex',
        breed: DogBreed.germanShepherd,
        stats:
            const DogStats(hunger: 80, happiness: 80, energy: 80, loyalty: 80),
        personality: const DogPersonality(protective: true, brave: true),
        createdAt: DateTime.now(),
        lastInteraction: DateTime.now(),
      );

      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            dogProvider.overrideWith((ref) => DogNotifier()..state = testDog),
          ],
          child: const DoggyDogsCarAlarmApp(),
        ),
      );

      await tester.pumpAndSettle();

      final materialApp = tester.widget<MaterialApp>(find.byType(MaterialApp));

      expect(materialApp.theme!.elevatedButtonTheme.style, isNotNull);
    });
  });

  group('App Integration Tests', () {
    setUp(() async {
      SharedPreferences.setMockInitialValues({});
    });

    testWidgets('full app smoke test - no dog path', (tester) async {
      await tester.pumpWidget(
        const ProviderScope(
          child: DoggyDogsCarAlarmApp(),
        ),
      );

      // Initial loading
      expect(find.byType(CircularProgressIndicator), findsOneWidget);

      // Wait for onboarding
      await tester.pump(const Duration(milliseconds: 500));
      await tester.pumpAndSettle();

      // Should show breed selection
      expect(find.byType(BreedSelectionScreen), findsOneWidget);
    });

    testWidgets('full app smoke test - with dog path', (tester) async {
      final testDog = Dog(
        id: 'integration-test',
        name: 'Max',
        breed: DogBreed.beagle,
        stats:
            const DogStats(hunger: 70, happiness: 70, energy: 70, loyalty: 70),
        personality: const DogPersonality(protective: true, brave: true),
        createdAt: DateTime.now(),
        lastInteraction: DateTime.now(),
      );

      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            dogProvider.overrideWith((ref) => DogNotifier()..state = testDog),
          ],
          child: const DoggyDogsCarAlarmApp(),
        ),
      );

      await tester.pumpAndSettle();

      // Should show home screen with dog
      expect(find.byType(HomeScreen), findsOneWidget);
    });

    testWidgets('app initializes with ProviderScope', (tester) async {
      final testDog = Dog(
        id: 'test',
        name: 'Rex',
        breed: DogBreed.germanShepherd,
        stats:
            const DogStats(hunger: 80, happiness: 80, energy: 80, loyalty: 80),
        personality: const DogPersonality(protective: true, brave: true),
        createdAt: DateTime.now(),
        lastInteraction: DateTime.now(),
      );

      await tester.pumpWidget(
        ProviderScope(
          overrides: [
            dogProvider.overrideWith((ref) => DogNotifier()..state = testDog),
          ],
          child: const DoggyDogsCarAlarmApp(),
        ),
      );

      await tester.pumpAndSettle();

      // Should not throw and should create the app
      expect(find.byType(DoggyDogsCarAlarmApp), findsOneWidget);
      expect(find.byType(MaterialApp), findsOneWidget);
    });
  });
}
