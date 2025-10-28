import 'dart:convert';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:shared_preferences/shared_preferences.dart';
import '../models/dog.dart';

/// Provider for managing the current guard dog
class DogNotifier extends StateNotifier<Dog?> {
  DogNotifier() : super(null) {
    _loadDog();
  }

  static const _dogKey = 'guard_dog';

  /// Load dog from storage
  Future<void> _loadDog() async {
    final prefs = await SharedPreferences.getInstance();
    final dogJson = prefs.getString(_dogKey);

    if (dogJson != null) {
      try {
        final dogMap = jsonDecode(dogJson) as Map<String, dynamic>;
        state = Dog.fromJson(dogMap);
      } catch (e) {
        // If parsing fails, keep state as null
        state = null;
      }
    }
  }

  /// Set and save dog
  Future<void> setDog(Dog dog) async {
    state = dog;
    await _saveDog();
  }

  /// Save current dog to storage
  Future<void> _saveDog() async {
    if (state == null) return;

    final prefs = await SharedPreferences.getInstance();
    final dogJson = jsonEncode(state!.toJson());
    await prefs.setString(_dogKey, dogJson);
  }

  /// Update dog stats
  Future<void> updateStats(DogStats newStats) async {
    if (state == null) return;

    state = state!.copyWith(
      stats: newStats,
      lastInteraction: DateTime.now(),
    );
    await _saveDog();
  }

  /// Feed the dog
  Future<void> feed() async {
    if (state == null) return;

    final newStats = state!.stats.copyWith(
      hunger: (state!.stats.hunger + 30).clamp(0, 100),
      happiness: (state!.stats.happiness + 5).clamp(0, 100),
    );

    await updateStats(newStats);
  }

  /// Play with the dog
  Future<void> play() async {
    if (state == null) return;

    final newStats = state!.stats.copyWith(
      happiness: (state!.stats.happiness + 20).clamp(0, 100),
      energy: (state!.stats.energy - 10).clamp(0, 100),
      loyalty: (state!.stats.loyalty + 5).clamp(0, 100),
    );

    await updateStats(newStats);
  }

  /// Let the dog rest
  Future<void> rest() async {
    if (state == null) return;

    final newStats = state!.stats.copyWith(
      energy: (state!.stats.energy + 40).clamp(0, 100),
    );

    await updateStats(newStats);
  }

  /// Add XP to the dog
  Future<void> addExperience(int xp) async {
    if (state == null) return;

    int newXp = state!.experience + xp;
    int newLevel = state!.level;

    // Check for level up
    while (newXp >= newLevel * 100 && newLevel < 50) {
      newXp -= newLevel * 100;
      newLevel++;
    }

    state = state!.copyWith(
      experience: newXp,
      level: newLevel,
    );
    await _saveDog();
  }

  /// Apply stat decay over time
  Future<void> applyDecay() async {
    if (state == null) return;

    final now = DateTime.now();
    final lastInteraction = state!.lastInteraction;
    final hoursSinceInteraction =
        now.difference(lastInteraction).inHours;

    if (hoursSinceInteraction > 0) {
      final decayAmount = hoursSinceInteraction * 2;
      final newStats = state!.stats.decay(
        hungerDecay: decayAmount,
        energyDecay: decayAmount ~/ 2,
        happinessDecay: decayAmount ~/ 3,
      );

      state = state!.copyWith(
        stats: newStats,
        lastInteraction: now,
      );
      await _saveDog();
    }
  }

  /// Clear the saved dog (for testing/reset)
  Future<void> clearDog() async {
    state = null;
    final prefs = await SharedPreferences.getInstance();
    await prefs.remove(_dogKey);
  }
}

/// Provider for the current guard dog
final dogProvider = StateNotifierProvider<DogNotifier, Dog?>((ref) {
  return DogNotifier();
});

/// Provider to check if user has completed onboarding
final hasCompletedOnboardingProvider = FutureProvider<bool>((ref) async {
  final dog = ref.watch(dogProvider);
  return dog != null;
});
