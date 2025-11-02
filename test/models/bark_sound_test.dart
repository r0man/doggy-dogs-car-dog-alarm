import 'package:flutter_test/flutter_test.dart';
import 'package:doggy_dogs_car_alarm/models/bark_sound.dart';
import 'package:doggy_dogs_car_alarm/models/dog.dart';

void main() {
  group('BarkSound', () {
    test('creates bark sound with correct properties', () {
      // Arrange & Act
      const bark = BarkSound(
        type: BarkType.warning,
        intensity: BarkIntensity.medium,
        breed: DogBreed.germanShepherd,
      );

      // Assert
      expect(bark.type, BarkType.warning);
      expect(bark.intensity, BarkIntensity.medium);
      expect(bark.breed, DogBreed.germanShepherd);
    });

    test('generates correct asset path', () {
      // Arrange
      const bark = BarkSound(
        type: BarkType.alert,
        intensity: BarkIntensity.high,
        breed: DogBreed.rottweiler,
      );

      // Act
      final path = bark.assetPath;

      // Assert
      expect(path, 'sounds/rottweiler/alert_high.mp3');
    });

    test('toString returns readable representation', () {
      // Arrange
      const bark = BarkSound(
        type: BarkType.aggressive,
        intensity: BarkIntensity.maximum,
        breed: DogBreed.doberman,
      );

      // Act
      final str = bark.toString();

      // Assert
      expect(str, contains('BarkSound'));
      expect(str, contains('doberman'));
      expect(str, contains('aggressive'));
      expect(str, contains('maximum'));
    });
  });

  group('BarkType', () {
    test('all bark types have display names', () {
      for (final type in BarkType.values) {
        expect(type.displayName.isNotEmpty, true);
      }
    });

    test('all bark types have descriptions', () {
      for (final type in BarkType.values) {
        expect(type.description.isNotEmpty, true);
      }
    });

    test(
        'duration is consistent across all bark types (current implementation)',
        () {
      // All sounds currently normalized to 3 seconds
      // TODO: Update when duration-specific variants are implemented
      expect(BarkType.warning.duration, 3.0);
      expect(BarkType.alert.duration, 3.0);
      expect(BarkType.aggressive.duration, 3.0);
      expect(BarkType.threat.duration, 3.0);
    });

    // Commented out - will be re-enabled when duration-specific variants are added
    // test('duration increases with bark severity', () {
    //   expect(BarkType.warning.duration, lessThan(BarkType.alert.duration));
    //   expect(BarkType.alert.duration, lessThan(BarkType.aggressive.duration));
    //   expect(
    //     BarkType.aggressive.duration,
    //     lessThan(BarkType.threat.duration),
    //   );
    // });

    // test('warning bark has shortest duration', () {
    //   expect(BarkType.warning.duration, 1.0);
    // });

    // test('threat bark has longest duration', () {
    //   expect(BarkType.threat.duration, 8.0);
    // });
  });

  group('BarkIntensity', () {
    test('all intensities have display names', () {
      for (final intensity in BarkIntensity.values) {
        expect(intensity.displayName.isNotEmpty, true);
      }
    });

    test('volume increases with intensity', () {
      expect(BarkIntensity.low.volume, lessThan(BarkIntensity.medium.volume));
      expect(
        BarkIntensity.medium.volume,
        lessThan(BarkIntensity.high.volume),
      );
      expect(
        BarkIntensity.high.volume,
        lessThan(BarkIntensity.maximum.volume),
      );
    });

    test('low intensity has quietest volume', () {
      expect(BarkIntensity.low.volume, 0.4);
    });

    test('maximum intensity has loudest volume', () {
      expect(BarkIntensity.maximum.volume, 1.0);
    });

    test('all volumes are in valid range', () {
      for (final intensity in BarkIntensity.values) {
        expect(intensity.volume, greaterThanOrEqualTo(0.0));
        expect(intensity.volume, lessThanOrEqualTo(1.0));
      }
    });
  });

  group('BarkLevel', () {
    test('creates bark level with properties', () {
      // Arrange & Act
      const level = BarkLevel(
        type: BarkType.alert,
        intensity: BarkIntensity.high,
        repeatCount: 3,
      );

      // Assert
      expect(level.type, BarkType.alert);
      expect(level.intensity, BarkIntensity.high);
      expect(level.repeatCount, 3);
    });

    test('default repeat count is 1', () {
      // Arrange & Act
      const level = BarkLevel(
        type: BarkType.warning,
        intensity: BarkIntensity.low,
      );

      // Assert
      expect(level.repeatCount, 1);
    });

    test('converts to bark sound with breed', () {
      // Arrange
      const level = BarkLevel(
        type: BarkType.aggressive,
        intensity: BarkIntensity.maximum,
      );

      // Act
      final bark = level.toBarkSound(DogBreed.pitbull);

      // Assert
      expect(bark.type, BarkType.aggressive);
      expect(bark.intensity, BarkIntensity.maximum);
      expect(bark.breed, DogBreed.pitbull);
    });

    test('toString returns readable representation', () {
      // Arrange
      const level = BarkLevel(
        type: BarkType.threat,
        intensity: BarkIntensity.high,
        repeatCount: 2,
      );

      // Act
      final str = level.toString();

      // Assert
      expect(str, contains('BarkLevel'));
      expect(str, contains('threat'));
      expect(str, contains('high'));
      expect(str, contains('x2'));
    });
  });

  group('BarkEscalation', () {
    test('creates escalation with levels', () {
      // Arrange
      final escalation = BarkEscalation(
        levels: const [
          BarkLevel(type: BarkType.warning, intensity: BarkIntensity.low),
          BarkLevel(type: BarkType.alert, intensity: BarkIntensity.medium),
        ],
      );

      // Assert
      expect(escalation.levels.length, 2);
      expect(escalation.currentLevel, 0);
    });

    test('current returns first level initially', () {
      // Arrange
      final escalation = BarkEscalation(
        levels: const [
          BarkLevel(type: BarkType.warning, intensity: BarkIntensity.low),
          BarkLevel(type: BarkType.alert, intensity: BarkIntensity.medium),
        ],
      );

      // Act
      final current = escalation.current;

      // Assert
      expect(current.type, BarkType.warning);
      expect(current.intensity, BarkIntensity.low);
    });

    test('escalate moves to next level', () {
      // Arrange
      final escalation = BarkEscalation(
        levels: const [
          BarkLevel(type: BarkType.warning, intensity: BarkIntensity.low),
          BarkLevel(type: BarkType.alert, intensity: BarkIntensity.medium),
        ],
      );

      // Act
      final didEscalate = escalation.escalate();
      final current = escalation.current;

      // Assert
      expect(didEscalate, true);
      expect(current.type, BarkType.alert);
      expect(current.intensity, BarkIntensity.medium);
    });

    test('escalate returns false at max level', () {
      // Arrange
      final escalation = BarkEscalation(
        levels: const [
          BarkLevel(type: BarkType.warning, intensity: BarkIntensity.low),
        ],
      );

      // Act
      final didEscalate = escalation.escalate();

      // Assert
      expect(didEscalate, false);
    });

    test('isMaxLevel returns true at last level', () {
      // Arrange
      final escalation = BarkEscalation(
        levels: const [
          BarkLevel(type: BarkType.warning, intensity: BarkIntensity.low),
          BarkLevel(type: BarkType.alert, intensity: BarkIntensity.medium),
        ],
      );

      // Act & Assert
      expect(escalation.isMaxLevel, false);
      escalation.escalate();
      expect(escalation.isMaxLevel, true);
    });

    test('reset returns to first level', () {
      // Arrange
      final escalation = BarkEscalation(
        levels: const [
          BarkLevel(type: BarkType.warning, intensity: BarkIntensity.low),
          BarkLevel(type: BarkType.alert, intensity: BarkIntensity.medium),
          BarkLevel(type: BarkType.aggressive, intensity: BarkIntensity.high),
        ],
      );

      escalation.escalate();
      escalation.escalate();
      expect(escalation.currentLevel, 2);

      // Act
      escalation.reset();

      // Assert
      expect(escalation.currentLevel, 0);
      expect(escalation.current.type, BarkType.warning);
    });

    test('standard escalation has 4 levels', () {
      // Arrange & Act
      final escalation = BarkEscalation.standard;

      // Assert
      expect(escalation.levels.length, 4);
      expect(escalation.levels[0].type, BarkType.warning);
      expect(escalation.levels[1].type, BarkType.alert);
      expect(escalation.levels[2].type, BarkType.aggressive);
      expect(escalation.levels[3].type, BarkType.threat);
    });

    test('aggressive escalation has 3 levels', () {
      // Arrange & Act
      final escalation = BarkEscalation.aggressive;

      // Assert
      expect(escalation.levels.length, 3);
      expect(escalation.levels[0].type, BarkType.alert);
      expect(escalation.levels[1].type, BarkType.aggressive);
      expect(escalation.levels[2].type, BarkType.threat);
    });

    test('aggressive escalation has shorter intervals', () {
      // Arrange & Act
      final standard = BarkEscalation.standard;
      final aggressive = BarkEscalation.aggressive;

      // Assert
      expect(
        aggressive.timeBetweenLevels,
        lessThan(standard.timeBetweenLevels),
      );
    });

    test('stealth escalation has minimal barking', () {
      // Arrange & Act
      final escalation = BarkEscalation.stealth;

      // Assert
      expect(escalation.levels.length, 1);
      expect(escalation.levels[0].intensity, BarkIntensity.low);
    });
  });

  group('Bark escalation progression', () {
    test('can escalate through all levels', () {
      // Arrange
      final escalation = BarkEscalation.standard;

      // Act & Assert
      expect(escalation.current.type, BarkType.warning);

      escalation.escalate();
      expect(escalation.current.type, BarkType.alert);

      escalation.escalate();
      expect(escalation.current.type, BarkType.aggressive);

      escalation.escalate();
      expect(escalation.current.type, BarkType.threat);

      // At max, should stay at threat
      final didEscalate = escalation.escalate();
      expect(didEscalate, false);
      expect(escalation.current.type, BarkType.threat);
    });

    test('intensity increases with escalation', () {
      // Arrange
      final escalation = BarkEscalation.standard;

      // Act & Assert
      expect(escalation.current.intensity, BarkIntensity.low);

      escalation.escalate();
      expect(escalation.current.intensity, BarkIntensity.medium);

      escalation.escalate();
      expect(escalation.current.intensity, BarkIntensity.high);

      escalation.escalate();
      expect(escalation.current.intensity, BarkIntensity.maximum);
    });
  });
}
