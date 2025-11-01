import 'package:flutter/material.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:doggy_dogs_car_alarm/screens/home_screen.dart';
import 'package:doggy_dogs_car_alarm/models/dog.dart';
import 'package:doggy_dogs_car_alarm/providers/dog_provider.dart';
import 'package:doggy_dogs_car_alarm/widgets/animated_dog_widget.dart';
import 'package:shared_preferences/shared_preferences.dart';

void main() {
  group('HomeScreen Widget Tests', () {
    setUp(() async {
      SharedPreferences.setMockInitialValues({});
    });

    testWidgets('shows loading indicator when no dog', (tester) async {
      await tester.pumpWidget(
        const ProviderScope(
          child: MaterialApp(
            home: HomeScreen(),
          ),
        ),
      );

      expect(find.byType(CircularProgressIndicator), findsOneWidget);
    });

    testWidgets('displays dog name in app bar when dog exists', (tester) async {
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
            home: HomeScreen(),
          ),
        ),
      );

      await tester.pumpAndSettle();

      expect(find.text('🐕 Rex'), findsOneWidget);
    });

    testWidgets('displays settings button', (tester) async {
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
            home: HomeScreen(),
          ),
        ),
      );

      await tester.pumpAndSettle();

      expect(find.byIcon(Icons.settings), findsOneWidget);
      expect(find.byTooltip('Settings'), findsOneWidget);
    });

    testWidgets('displays change breed button', (tester) async {
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
            home: HomeScreen(),
          ),
        ),
      );

      await tester.pumpAndSettle();

      expect(find.byIcon(Icons.pets), findsOneWidget);
      expect(find.byTooltip('Change Breed'), findsOneWidget);
    });

    testWidgets('displays animated dog widget', (tester) async {
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
            home: HomeScreen(),
          ),
        ),
      );

      await tester.pumpAndSettle();

      // AnimatedDogWidget should be present
      expect(find.byType(AnimatedDogWidget), findsOneWidget);
    });

    testWidgets('has settings button with tooltip', (tester) async {
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
            home: HomeScreen(),
          ),
        ),
      );

      await tester.pumpAndSettle();

      // Verify settings button exists
      expect(find.byIcon(Icons.settings), findsOneWidget);
      expect(find.byTooltip('Settings'), findsOneWidget);
    });

    testWidgets('displays dog stats', (tester) async {
      final testDog = Dog(
        id: 'test',
        name: 'Max',
        breed: DogBreed.germanShepherd,
        stats:
            const DogStats(hunger: 75, happiness: 85, energy: 90, loyalty: 95),
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
            home: HomeScreen(),
          ),
        ),
      );

      await tester.pumpAndSettle();

      // Check for stat labels (only Hunger, Happiness, Energy are shown)
      expect(find.text('Hunger'), findsOneWidget);
      expect(find.text('Happiness'), findsOneWidget);
      expect(find.text('Energy'), findsOneWidget);
    });

    testWidgets('displays action buttons', (tester) async {
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
            home: HomeScreen(),
          ),
        ),
      );

      await tester.pumpAndSettle();

      // Check for action buttons (only Feed and Play)
      expect(find.text('Feed'), findsOneWidget);
      expect(find.text('Play'), findsOneWidget);
    });

    testWidgets('displays alarm button', (tester) async {
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
            home: HomeScreen(),
          ),
        ),
      );

      await tester.pumpAndSettle();

      // Check for alarm activation button
      expect(find.byIcon(Icons.security), findsWidgets);
    });
  });
}
