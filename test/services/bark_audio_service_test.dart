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
      const breed = DogBreed.germanShepherd;

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
            test('${type.name}_${intensity.name}.mp3 exists and loads',
                () async {
              final sound = BarkSound(
                type: type,
                intensity: intensity,
                breed: breed,
              );

              // Directly load the asset and verify it exists
              final data = await rootBundle.load(sound.assetPath);

              expect(
                data.lengthInBytes,
                greaterThan(0),
                reason:
                    'Asset file should exist and have content: ${sound.assetPath}',
              );

              // Verify reasonable file size (between 10KB and 100KB for 3-second MP3)
              expect(
                data.lengthInBytes,
                inInclusiveRange(10000, 100000),
                reason: 'Asset file size should be reasonable for 3-second MP3',
              );
            });
          }
        }
      });
    }
  });

  group('BarkSound Model', () {
    test('toString returns correct format', () {
      const sound = BarkSound(
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
      // All sounds currently normalized to 3 seconds
      // TODO: Update when duration-specific variants are implemented
      expect(BarkType.warning.duration, 3.0);
      expect(BarkType.alert.duration, 3.0);
      expect(BarkType.aggressive.duration, 3.0);
      expect(BarkType.threat.duration, 3.0);

      // Future expected values when variants are added:
      // expect(BarkType.warning.duration, 1.0);
      // expect(BarkType.alert.duration, 3.0);
      // expect(BarkType.aggressive.duration, 5.0);
      // expect(BarkType.threat.duration, 8.0);
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
