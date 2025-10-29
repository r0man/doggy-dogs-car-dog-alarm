import 'dart:convert';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:shared_preferences/shared_preferences.dart';
import '../models/dog.dart';
import '../models/dog_backstory.dart';
import '../services/app_settings_service.dart';
import 'dog_provider.dart';

/// Backstory service
class BackstoryService {
  static const String _backstoryKey = 'dog_backstory';

  final SharedPreferences _prefs;

  BackstoryService(this._prefs);

  /// Get saved backstory
  DogBackstory? getBackstory() {
    final backstoryJson = _prefs.getString(_backstoryKey);
    if (backstoryJson == null) return null;

    try {
      final Map<String, dynamic> json = jsonDecode(backstoryJson);
      return DogBackstory.fromJson(json);
    } catch (e) {
      return null;
    }
  }

  /// Save backstory
  Future<void> saveBackstory(DogBackstory backstory) async {
    final backstoryJson = jsonEncode(backstory.toJson());
    await _prefs.setString(_backstoryKey, backstoryJson);
  }

  /// Generate and save new backstory
  Future<DogBackstory> generateBackstory({
    required String name,
    required DogBreed breed,
  }) async {
    final backstory = DogBackstory.generate(
      name: name,
      breed: breed,
      seed: name.hashCode + breed.hashCode, // Consistent generation
    );

    await saveBackstory(backstory);
    return backstory;
  }

  /// Clear backstory
  Future<void> clearBackstory() async {
    await _prefs.remove(_backstoryKey);
  }
}

/// Provider for backstory service
final backstoryServiceProvider = Provider<BackstoryService>((ref) {
  final prefs = ref.watch(sharedPreferencesProvider);
  return BackstoryService(prefs);
});

/// Current backstory provider
final backstoryProvider = FutureProvider<DogBackstory?>((ref) async {
  final dog = ref.watch(dogProvider);
  if (dog == null) return null;

  final service = ref.watch(backstoryServiceProvider);
  var backstory = service.getBackstory();

  // Generate if doesn't exist
  backstory ??= await service.generateBackstory(
    name: dog.name,
    breed: dog.breed,
  );

  return backstory;
});
