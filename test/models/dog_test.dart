import 'package:flutter_test/flutter_test.dart';
import 'package:doggy_dogs_car_alarm/models/dog.dart';

void main() {
  group('Dog Model Tests', () {
    test('Dog creation with default values', () {
      final now = DateTime.now();
      final dog = Dog(
        id: '1',
        name: 'Max',
        breed: DogBreed.germanShepherd,
        stats: const DogStats(
          hunger: 100,
          happiness: 100,
          energy: 100,
          loyalty: 50,
        ),
        personality: const DogPersonality(brave: true, protective: true),
        createdAt: now,
        lastInteraction: now,
      );

      expect(dog.level, 1);
      expect(dog.experience, 0);
      expect(dog.currentMood, DogMood.content);
    });

    test('XP calculation for next level', () {
      final now = DateTime.now();
      final dog = Dog(
        id: '1',
        name: 'Max',
        breed: DogBreed.germanShepherd,
        level: 5,
        stats: const DogStats(
          hunger: 100,
          happiness: 100,
          energy: 100,
          loyalty: 50,
        ),
        personality: const DogPersonality(),
        createdAt: now,
        lastInteraction: now,
      );

      expect(dog.xpForNextLevel, 500); // level * 100
    });

    test('Dog is neglected when stats are low', () {
      final now = DateTime.now();
      final dog = Dog(
        id: '1',
        name: 'Max',
        breed: DogBreed.germanShepherd,
        stats: const DogStats(
          hunger: 20, // Low hunger
          happiness: 80,
          energy: 80,
          loyalty: 50,
        ),
        personality: const DogPersonality(),
        createdAt: now,
        lastInteraction: now,
      );

      expect(dog.isNeglected, true);
    });

    test('Dog is not neglected when stats are good', () {
      final now = DateTime.now();
      final dog = Dog(
        id: '1',
        name: 'Max',
        breed: DogBreed.germanShepherd,
        stats: const DogStats(
          hunger: 80,
          happiness: 80,
          energy: 80,
          loyalty: 50,
        ),
        personality: const DogPersonality(),
        createdAt: now,
        lastInteraction: now,
      );

      expect(dog.isNeglected, false);
    });

    test('Effectiveness is reduced when neglected', () {
      final now = DateTime.now();
      final wellCaredDog = Dog(
        id: '1',
        name: 'Max',
        breed: DogBreed.germanShepherd,
        stats: const DogStats(
          hunger: 90,
          happiness: 90,
          energy: 90,
          loyalty: 50,
        ),
        personality: const DogPersonality(),
        createdAt: now,
        lastInteraction: now,
      );

      final neglectedDog = Dog(
        id: '2',
        name: 'Rex',
        breed: DogBreed.rottweiler,
        stats: const DogStats(
          hunger: 20,
          happiness: 20,
          energy: 20,
          loyalty: 50,
        ),
        personality: const DogPersonality(),
        createdAt: now,
        lastInteraction: now,
      );

      expect(wellCaredDog.effectiveness, greaterThan(80));
      expect(
          neglectedDog.effectiveness, 30); // Fixed effectiveness when neglected
    });

    test('Stats decay over time', () {
      const stats = DogStats(
        hunger: 100,
        happiness: 100,
        energy: 100,
        loyalty: 100,
      );

      final decayedStats = stats.decay();

      expect(decayedStats.hunger, 95); // Default decay of 5
      expect(decayedStats.happiness, 98); // Default decay of 2
      expect(decayedStats.energy, 97); // Default decay of 3
      expect(decayedStats.loyalty, 100); // Loyalty doesn't decay
    });

    test('Stats cannot go below 0', () {
      const stats = DogStats(
        hunger: 2,
        happiness: 1,
        energy: 1,
        loyalty: 50,
      );

      final decayedStats = stats.decay();

      expect(decayedStats.hunger, 0);
      expect(decayedStats.happiness, 0);
      expect(decayedStats.energy, 0);
    });
  });

  group('DogBreed Extension Tests', () {
    test('Display names are correct', () {
      expect(DogBreed.germanShepherd.displayName, 'German Shepherd');
      expect(DogBreed.husky.displayName, 'Husky');
    });

    test('Asset paths are correct', () {
      expect(
          DogBreed.germanShepherd.assetPath, 'assets/dogs/german_shepherd.svg');
      expect(DogBreed.husky.assetPath, 'assets/dogs/husky.svg');
    });
  });

  group('DogMood Extension Tests', () {
    test('Mood emojis are assigned', () {
      expect(DogMood.happy.emoji, '😊');
      expect(DogMood.sad.emoji, '😢');
      expect(DogMood.grumpy.emoji, '😠');
    });

    test('Mood descriptions are assigned', () {
      expect(DogMood.happy.description, 'Your dog is happy!');
      expect(DogMood.sleeping.description, 'Your dog is sleeping');
    });
  });
}
