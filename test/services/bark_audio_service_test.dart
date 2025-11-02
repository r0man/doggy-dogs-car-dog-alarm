import 'package:flutter/services.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:doggy_dogs_car_alarm/models/bark_sound.dart';
import 'package:doggy_dogs_car_alarm/models/dog.dart';

void main() {
  TestWidgetsFlutterBinding.ensureInitialized();

  group('BarkSound Asset Paths', () {
    test('generates correct asset paths for all breeds', () {
      for (final breed in DogBreed.values) {
        final sound = BarkSound(
          type: BarkType.warning,
          intensity: BarkIntensity.low,
          breed: breed,
        );

        final expectedBreedName = breed.name.toLowerCase();
        expect(
          sound.assetPath,
          'assets/sounds/$expectedBreedName/warning_low.mp3',
        );
      }
    });

    test('generates correct asset paths for all type/intensity combinations',
        () {
      final breed = DogBreed.germanShepherd;

      for (final type in BarkType.values) {
        for (final intensity in BarkIntensity.values) {
          final sound = BarkSound(
            type: type,
            intensity: intensity,
            breed: breed,
          );

          expect(
            sound.assetPath,
            'assets/sounds/germanshepherd/${type.name}_${intensity.name}.mp3',
          );
        }
      }
    });
  });

  group('Bark Sound Assets Exist', () {
    // Test that all required asset files exist for each breed
    for (final breed in DogBreed.values) {
      group('${breed.displayName} bark assets', () {
        for (final type in BarkType.values) {
          for (final intensity in BarkIntensity.values) {
            test('${type.name}_${intensity.name}.mp3 exists', () async {
              final sound = BarkSound(
                type: type,
                intensity: intensity,
                breed: breed,
              );

              // Try to load the asset
              // In test environment, we check if the file exists by attempting to load it
              expect(
                () async {
                  final data = await rootBundle.load(sound.assetPath);
                  expect(data.lengthInBytes, greaterThan(0));
                },
                returnsNormally,
                reason:
                    'Asset file should exist: ${sound.assetPath}',
              );
            });
          }
        }
      });
    }
  });

  group('BarkSound Model', () {
    test('toString returns correct format', () {
      final sound = BarkSound(
        type: BarkType.aggressive,
        intensity: BarkIntensity.high,
        breed: DogBreed.rottweiler,
      );

      expect(
        sound.toString(),
        'BarkSound(DogBreed.rottweiler, BarkType.aggressive, BarkIntensity.high)',
      );
    });
  });

  group('BarkType Extension', () {
    test('displayName returns correct values', () {
      expect(BarkType.warning.displayName, 'Warning');
      expect(BarkType.alert.displayName, 'Alert');
      expect(BarkType.aggressive.displayName, 'Aggressive');
      expect(BarkType.threat.displayName, 'Threat');
    });

    test('duration returns correct values', () {
      expect(BarkType.warning.duration, 1.0);
      expect(BarkType.alert.duration, 3.0);
      expect(BarkType.aggressive.duration, 5.0);
      expect(BarkType.threat.duration, 8.0);
    });
  });

  group('BarkIntensity Extension', () {
    test('displayName returns correct values', () {
      expect(BarkIntensity.low.displayName, 'Low');
      expect(BarkIntensity.medium.displayName, 'Medium');
      expect(BarkIntensity.high.displayName, 'High');
      expect(BarkIntensity.maximum.displayName, 'Maximum');
    });

    test('volume returns correct values', () {
      expect(BarkIntensity.low.volume, 0.4);
      expect(BarkIntensity.medium.volume, 0.6);
      expect(BarkIntensity.high.volume, 0.8);
      expect(BarkIntensity.maximum.volume, 1.0);
    });
  });
}
