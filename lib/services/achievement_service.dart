import 'dart:convert';
import 'package:shared_preferences/shared_preferences.dart';
import '../models/achievement.dart';

/// Service for tracking and unlocking achievements
class AchievementService {
  static const String _progressKey = 'achievement_progress';

  final SharedPreferences _prefs;

  AchievementService(this._prefs);

  /// Get all achievement progress
  Map<String, AchievementProgress> getAllProgress() {
    final progressJson = _prefs.getString(_progressKey);
    if (progressJson == null) return {};

    try {
      final Map<String, dynamic> json = jsonDecode(progressJson);
      return json.map((key, value) =>
          MapEntry(key, AchievementProgress.fromJson(value as Map<String, dynamic>)));
    } catch (e) {
      return {};
    }
  }

  /// Get progress for specific achievement
  AchievementProgress getProgress(String achievementId) {
    final allProgress = getAllProgress();
    return allProgress[achievementId] ??
        AchievementProgress(
          achievementId: achievementId,
          currentProgress: 0,
        );
  }

  /// Save progress
  Future<void> _saveProgress(Map<String, AchievementProgress> progress) async {
    final json = progress.map((key, value) => MapEntry(key, value.toJson()));
    await _prefs.setString(_progressKey, jsonEncode(json));
  }

  /// Increment progress for an achievement
  Future<bool> incrementProgress(String achievementId, {int amount = 1}) async {
    final achievement = Achievements.getById(achievementId);
    if (achievement == null) return false;

    final allProgress = getAllProgress();
    final currentProgress = getProgress(achievementId);

    if (currentProgress.isUnlocked) return false; // Already unlocked

    final newProgress = currentProgress.currentProgress + amount;
    final shouldUnlock = newProgress >= achievement.requirement;

    allProgress[achievementId] = AchievementProgress(
      achievementId: achievementId,
      currentProgress: newProgress,
      unlockedAt: shouldUnlock ? DateTime.now() : null,
    );

    await _saveProgress(allProgress);
    return shouldUnlock;
  }

  /// Get unlocked achievements
  List<Achievement> getUnlockedAchievements() {
    final allProgress = getAllProgress();
    return Achievements.all.where((a) => allProgress[a.id]?.isUnlocked ?? false).toList();
  }

  /// Get achievement count by category
  Map<AchievementCategory, int> getUnlockedCountByCategory() {
    final unlocked = getUnlockedAchievements();
    final Map<AchievementCategory, int> counts = {};

    for (final achievement in unlocked) {
      counts[achievement.category] = (counts[achievement.category] ?? 0) + 1;
    }

    return counts;
  }

  /// Check if achievement is unlocked
  bool isUnlocked(String achievementId) {
    return getProgress(achievementId).isUnlocked;
  }

  /// Get completion percentage
  double getCompletionPercentage() {
    final unlocked = getUnlockedAchievements().length;
    final total = Achievements.all.length;
    return unlocked / total;
  }
}
