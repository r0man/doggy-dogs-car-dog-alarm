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

  group('Dog copyWith Tests', () {
    test('copyWith creates new instance with updated values', () {
      final now = DateTime.now();
      final original = Dog(
        id: '1',
        name: 'Max',
        breed: DogBreed.germanShepherd,
        level: 5,
        experience: 200,
        stats: const DogStats(
          hunger: 80,
          happiness: 80,
          energy: 80,
          loyalty: 50,
        ),
        personality: const DogPersonality(brave: true),
        currentMood: DogMood.content,
        createdAt: now,
        lastInteraction: now,
      );

      final updated = original.copyWith(
        name: 'Rex',
        level: 6,
        experience: 250,
        currentMood: DogMood.happy,
      );

      expect(updated.id, original.id);
      expect(updated.name, 'Rex');
      expect(updated.level, 6);
      expect(updated.experience, 250);
      expect(updated.currentMood, DogMood.happy);
      expect(updated.breed, original.breed);
    });

    test('copyWith without parameters returns identical dog', () {
      final now = DateTime.now();
      final original = Dog(
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

      final copy = original.copyWith();

      expect(copy.id, original.id);
      expect(copy.name, original.name);
      expect(copy.level, original.level);
    });
  });

  group('Dog JSON Serialization Tests', () {
    test('toJson creates correct map', () {
      final now = DateTime.now();
      final dog = Dog(
        id: '123',
        name: 'Buddy',
        breed: DogBreed.bulldog,
        level: 3,
        experience: 150,
        stats: const DogStats(
          hunger: 70,
          happiness: 80,
          energy: 90,
          loyalty: 60,
        ),
        personality: const DogPersonality(playful: true, energetic: true),
        currentMood: DogMood.excited,
        createdAt: now,
        lastInteraction: now,
      );

      final json = dog.toJson();

      expect(json['id'], '123');
      expect(json['name'], 'Buddy');
      expect(json['breed'], 'bulldog');
      expect(json['level'], 3);
      expect(json['experience'], 150);
      expect(json['currentMood'], 'excited');
      expect(json['stats'], isA<Map<String, dynamic>>());
      expect(json['personality'], isA<Map<String, dynamic>>());
    });

    test('fromJson creates correct Dog instance', () {
      final now = DateTime.now();
      final json = {
        'id': '456',
        'name': 'Charlie',
        'breed': 'husky',
        'level': 7,
        'experience': 600,
        'stats': {
          'hunger': 85,
          'happiness': 75,
          'energy': 95,
          'loyalty': 80,
        },
        'personality': {
          'brave': true,
          'nervous': false,
          'playful': true,
          'lazy': false,
          'energetic': true,
          'protective': false,
        },
        'currentMood': 'alert',
        'createdAt': now.toIso8601String(),
        'lastInteraction': now.toIso8601String(),
      };

      final dog = Dog.fromJson(json);

      expect(dog.id, '456');
      expect(dog.name, 'Charlie');
      expect(dog.breed, DogBreed.husky);
      expect(dog.level, 7);
      expect(dog.experience, 600);
      expect(dog.currentMood, DogMood.alert);
      expect(dog.stats.hunger, 85);
      expect(dog.personality.playful, true);
    });

    test('Dog round-trip JSON serialization', () {
      final now = DateTime.now();
      final original = Dog(
        id: '789',
        name: 'Luna',
        breed: DogBreed.beagle,
        level: 10,
        experience: 900,
        stats: const DogStats(
          hunger: 100,
          happiness: 100,
          energy: 100,
          loyalty: 100,
        ),
        personality: const DogPersonality(protective: true, brave: true),
        currentMood: DogMood.sleeping,
        createdAt: now,
        lastInteraction: now,
      );

      final json = original.toJson();
      final restored = Dog.fromJson(json);

      expect(restored.id, original.id);
      expect(restored.name, original.name);
      expect(restored.breed, original.breed);
      expect(restored.level, original.level);
      expect(restored.experience, original.experience);
      expect(restored.currentMood, original.currentMood);
    });
  });

  group('DogBreed description Tests', () {
    test('All breeds have descriptions', () {
      expect(DogBreed.germanShepherd.description,
          'Loyal and alert with characteristic tan and black coloring');
      expect(DogBreed.rottweiler.description,
          'Strong protector with distinctive markings');
      expect(DogBreed.doberman.description,
          'Sleek and elegant with alert expression');
      expect(DogBreed.bulldog.description,
          'Stocky and determined with adorable wrinkles');
      expect(DogBreed.pitbull.description,
          'Muscular and friendly with a big smile');
      expect(DogBreed.husky.description,
          'Striking blue eyes and fluffy appearance');
      expect(
          DogBreed.beagle.description, 'Sweet and curious with tri-color coat');
    });
  });

  group('DogStats copyWith Tests', () {
    test('copyWith updates specified stats', () {
      const original = DogStats(
        hunger: 80,
        happiness: 70,
        energy: 60,
        loyalty: 50,
      );

      final updated = original.copyWith(hunger: 100, energy: 90);

      expect(updated.hunger, 100);
      expect(updated.happiness, 70);
      expect(updated.energy, 90);
      expect(updated.loyalty, 50);
    });
  });

  group('DogStats JSON Serialization Tests', () {
    test('toJson creates correct map', () {
      const stats = DogStats(
        hunger: 85,
        happiness: 90,
        energy: 75,
        loyalty: 95,
      );

      final json = stats.toJson();

      expect(json['hunger'], 85);
      expect(json['happiness'], 90);
      expect(json['energy'], 75);
      expect(json['loyalty'], 95);
    });

    test('fromJson creates correct DogStats', () {
      final json = {
        'hunger': 70,
        'happiness': 80,
        'energy': 90,
        'loyalty': 60,
      };

      final stats = DogStats.fromJson(json);

      expect(stats.hunger, 70);
      expect(stats.happiness, 80);
      expect(stats.energy, 90);
      expect(stats.loyalty, 60);
    });
  });

  group('DogPersonality JSON Serialization Tests', () {
    test('toJson creates correct map', () {
      const personality = DogPersonality(
        brave: true,
        nervous: false,
        playful: true,
        lazy: false,
        energetic: true,
        protective: false,
      );

      final json = personality.toJson();

      expect(json['brave'], true);
      expect(json['nervous'], false);
      expect(json['playful'], true);
      expect(json['lazy'], false);
      expect(json['energetic'], true);
      expect(json['protective'], false);
    });

    test('fromJson creates correct DogPersonality', () {
      final json = {
        'brave': true,
        'nervous': true,
        'playful': false,
        'lazy': true,
        'energetic': false,
        'protective': true,
      };

      final personality = DogPersonality.fromJson(json);

      expect(personality.brave, true);
      expect(personality.nervous, true);
      expect(personality.playful, false);
      expect(personality.lazy, true);
      expect(personality.energetic, false);
      expect(personality.protective, true);
    });

    test('fromJson handles missing fields', () {
      final json = <String, dynamic>{};

      final personality = DogPersonality.fromJson(json);

      expect(personality.brave, false);
      expect(personality.nervous, false);
      expect(personality.playful, false);
      expect(personality.lazy, false);
      expect(personality.energetic, false);
      expect(personality.protective, false);
    });
  });

  group('Dog xpProgress Tests', () {
    test('xpProgress calculates correctly', () {
      final now = DateTime.now();
      final dog = Dog(
        id: '1',
        name: 'Max',
        breed: DogBreed.germanShepherd,
        level: 5,
        experience: 250, // level * 100 = 500 needed
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

      expect(dog.xpProgress, 0.5); // 250 / 500 = 0.5
    });

    test('xpProgress at zero experience', () {
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

      expect(dog.xpProgress, 0.0);
    });
  });
}
