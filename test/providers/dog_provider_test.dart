import 'package:flutter_test/flutter_test.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:doggy_dogs_car_alarm/providers/dog_provider.dart';
import 'package:doggy_dogs_car_alarm/models/dog.dart';
import 'package:shared_preferences/shared_preferences.dart';

void main() {
  group('DogNotifier Tests', () {
    setUp(() async {
      // Clear shared preferences before each test
      SharedPreferences.setMockInitialValues({});
    });

    test('initial state is null when no dog saved', () async {
      final container = ProviderContainer();
      addTearDown(container.dispose);

      // Wait for initial load
      await Future.delayed(const Duration(milliseconds: 100));

      final dog = container.read(dogProvider);
      expect(dog, isNull);
    });

    test('setDog sets and saves dog', () async {
      final container = ProviderContainer();
      addTearDown(container.dispose);

      final testDog = Dog(
        id: 'test-dog',
        name: 'Rex',
        breed: DogBreed.germanShepherd,
        stats:
            const DogStats(hunger: 80, happiness: 80, energy: 80, loyalty: 80),
        personality: const DogPersonality(protective: true, brave: true),
        createdAt: DateTime.now(),
        lastInteraction: DateTime.now(),
      );

      await container.read(dogProvider.notifier).setDog(testDog);

      final savedDog = container.read(dogProvider);
      expect(savedDog, isNotNull);
      expect(savedDog!.id, testDog.id);
      expect(savedDog.name, testDog.name);
      expect(savedDog.breed, testDog.breed);
    });

    test('feed increases hunger and happiness', () async {
      final container = ProviderContainer();
      addTearDown(container.dispose);

      final testDog = Dog(
        id: 'test',
        name: 'Rex',
        breed: DogBreed.germanShepherd,
        stats:
            const DogStats(hunger: 50, happiness: 50, energy: 50, loyalty: 50),
        personality: const DogPersonality(protective: true, brave: true),
        createdAt: DateTime.now(),
        lastInteraction: DateTime.now(),
      );

      await container.read(dogProvider.notifier).setDog(testDog);
      await container.read(dogProvider.notifier).feed();

      final fedDog = container.read(dogProvider);
      expect(fedDog!.stats.hunger, 80);
      expect(fedDog.stats.happiness, 55);
    });

    test('play increases happiness and loyalty, decreases energy', () async {
      final container = ProviderContainer();
      addTearDown(container.dispose);

      final testDog = Dog(
        id: 'test',
        name: 'Rex',
        breed: DogBreed.germanShepherd,
        stats:
            const DogStats(hunger: 50, happiness: 50, energy: 50, loyalty: 50),
        personality: const DogPersonality(protective: true, brave: true),
        createdAt: DateTime.now(),
        lastInteraction: DateTime.now(),
      );

      await container.read(dogProvider.notifier).setDog(testDog);
      await container.read(dogProvider.notifier).play();

      final playedDog = container.read(dogProvider);
      expect(playedDog!.stats.happiness, 70);
      expect(playedDog.stats.energy, 40);
      expect(playedDog.stats.loyalty, 55);
    });

    test('rest increases energy', () async {
      final container = ProviderContainer();
      addTearDown(container.dispose);

      final testDog = Dog(
        id: 'test',
        name: 'Rex',
        breed: DogBreed.germanShepherd,
        stats:
            const DogStats(hunger: 50, happiness: 50, energy: 30, loyalty: 50),
        personality: const DogPersonality(protective: true, brave: true),
        createdAt: DateTime.now(),
        lastInteraction: DateTime.now(),
      );

      await container.read(dogProvider.notifier).setDog(testDog);
      await container.read(dogProvider.notifier).rest();

      final restedDog = container.read(dogProvider);
      expect(restedDog!.stats.energy, 70);
    });

    test('updateStats updates stats and lastInteraction', () async {
      final container = ProviderContainer();
      addTearDown(container.dispose);

      final testDog = Dog(
        id: 'test',
        name: 'Rex',
        breed: DogBreed.germanShepherd,
        stats:
            const DogStats(hunger: 50, happiness: 50, energy: 50, loyalty: 50),
        personality: const DogPersonality(protective: true, brave: true),
        createdAt: DateTime.now(),
        lastInteraction: DateTime.now().subtract(const Duration(hours: 1)),
      );

      await container.read(dogProvider.notifier).setDog(testDog);

      const newStats =
          DogStats(hunger: 90, happiness: 90, energy: 90, loyalty: 90);
      await container.read(dogProvider.notifier).updateStats(newStats);

      final updatedDog = container.read(dogProvider);
      expect(updatedDog!.stats.hunger, 90);
      expect(updatedDog.stats.happiness, 90);
      expect(updatedDog.stats.energy, 90);
      expect(updatedDog.stats.loyalty, 90);
    });

    test('addExperience increases XP and levels up', () async {
      final container = ProviderContainer();
      addTearDown(container.dispose);

      final testDog = Dog(
        id: 'test',
        name: 'Rex',
        breed: DogBreed.germanShepherd,
        stats:
            const DogStats(hunger: 50, happiness: 50, energy: 50, loyalty: 50),
        personality: const DogPersonality(protective: true, brave: true),
        createdAt: DateTime.now(),
        lastInteraction: DateTime.now(),
        level: 1,
        experience: 50,
      );

      await container.read(dogProvider.notifier).setDog(testDog);
      await container.read(dogProvider.notifier).addExperience(60);

      final leveledDog = container.read(dogProvider);
      expect(leveledDog!.level, 2);
      expect(leveledDog.experience, greaterThanOrEqualTo(0));
    });

    test('addExperience handles multiple level ups', () async {
      final container = ProviderContainer();
      addTearDown(container.dispose);

      final testDog = Dog(
        id: 'test',
        name: 'Rex',
        breed: DogBreed.germanShepherd,
        stats:
            const DogStats(hunger: 50, happiness: 50, energy: 50, loyalty: 50),
        personality: const DogPersonality(protective: true, brave: true),
        createdAt: DateTime.now(),
        lastInteraction: DateTime.now(),
        level: 1,
        experience: 0,
      );

      await container.read(dogProvider.notifier).setDog(testDog);
      await container.read(dogProvider.notifier).addExperience(500);

      final leveledDog = container.read(dogProvider);
      expect(leveledDog!.level, greaterThan(2));
    });

    test('applyDecay reduces stats based on time', () async {
      final container = ProviderContainer();
      addTearDown(container.dispose);

      final testDog = Dog(
        id: 'test',
        name: 'Rex',
        breed: DogBreed.germanShepherd,
        stats:
            const DogStats(hunger: 80, happiness: 80, energy: 80, loyalty: 80),
        personality: const DogPersonality(protective: true, brave: true),
        createdAt: DateTime.now().subtract(const Duration(hours: 5)),
        lastInteraction: DateTime.now().subtract(const Duration(hours: 5)),
      );

      await container.read(dogProvider.notifier).setDog(testDog);
      await container.read(dogProvider.notifier).applyDecay();

      final decayedDog = container.read(dogProvider);
      expect(decayedDog!.stats.hunger, lessThan(80));
      expect(decayedDog.stats.happiness, lessThan(80));
      expect(decayedDog.stats.energy, lessThan(80));
    });

    test('applyDecay does nothing if no time passed', () async {
      final container = ProviderContainer();
      addTearDown(container.dispose);

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

      await container.read(dogProvider.notifier).setDog(testDog);
      await container.read(dogProvider.notifier).applyDecay();

      final unchangedDog = container.read(dogProvider);
      expect(unchangedDog!.stats.hunger, 80);
      expect(unchangedDog.stats.happiness, 80);
      expect(unchangedDog.stats.energy, 80);
    });

    test('clearDog removes dog', () async {
      final container = ProviderContainer();
      addTearDown(container.dispose);

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

      await container.read(dogProvider.notifier).setDog(testDog);
      expect(container.read(dogProvider), isNotNull);

      await container.read(dogProvider.notifier).clearDog();
      expect(container.read(dogProvider), isNull);
    });

    test('feed does nothing when no dog', () async {
      final container = ProviderContainer();
      addTearDown(container.dispose);

      await container.read(dogProvider.notifier).feed();
      expect(container.read(dogProvider), isNull);
    });

    test('play does nothing when no dog', () async {
      final container = ProviderContainer();
      addTearDown(container.dispose);

      await container.read(dogProvider.notifier).play();
      expect(container.read(dogProvider), isNull);
    });

    test('rest does nothing when no dog', () async {
      final container = ProviderContainer();
      addTearDown(container.dispose);

      await container.read(dogProvider.notifier).rest();
      expect(container.read(dogProvider), isNull);
    });

    test('addExperience does nothing when no dog', () async {
      final container = ProviderContainer();
      addTearDown(container.dispose);

      await container.read(dogProvider.notifier).addExperience(100);
      expect(container.read(dogProvider), isNull);
    });

    test('updateStats does nothing when no dog', () async {
      final container = ProviderContainer();
      addTearDown(container.dispose);

      const newStats =
          DogStats(hunger: 90, happiness: 90, energy: 90, loyalty: 90);
      await container.read(dogProvider.notifier).updateStats(newStats);
      expect(container.read(dogProvider), isNull);
    });

    test('applyDecay does nothing when no dog', () async {
      final container = ProviderContainer();
      addTearDown(container.dispose);

      await container.read(dogProvider.notifier).applyDecay();
      expect(container.read(dogProvider), isNull);
    });

    test('stats clamp at 100 max', () async {
      final container = ProviderContainer();
      addTearDown(container.dispose);

      final testDog = Dog(
        id: 'test',
        name: 'Rex',
        breed: DogBreed.germanShepherd,
        stats:
            const DogStats(hunger: 90, happiness: 90, energy: 90, loyalty: 90),
        personality: const DogPersonality(protective: true, brave: true),
        createdAt: DateTime.now(),
        lastInteraction: DateTime.now(),
      );

      await container.read(dogProvider.notifier).setDog(testDog);
      await container.read(dogProvider.notifier).feed();

      final fedDog = container.read(dogProvider);
      expect(fedDog!.stats.hunger, 100);
      expect(fedDog.stats.happiness, lessThanOrEqualTo(100));
    });

    test('hasCompletedOnboardingProvider returns true when dog exists',
        () async {
      final container = ProviderContainer();
      addTearDown(container.dispose);

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

      await container.read(dogProvider.notifier).setDog(testDog);

      final hasCompleted =
          await container.read(hasCompletedOnboardingProvider.future);
      expect(hasCompleted, true);
    });

    test('hasCompletedOnboardingProvider returns false when no dog', () async {
      final container = ProviderContainer();
      addTearDown(container.dispose);

      // Wait for initial load
      await Future.delayed(const Duration(milliseconds: 100));

      final hasCompleted =
          await container.read(hasCompletedOnboardingProvider.future);
      expect(hasCompleted, false);
    });
  });
}
