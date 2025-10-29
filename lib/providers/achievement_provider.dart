import 'package:flutter_riverpod/flutter_riverpod.dart';
import '../models/achievement.dart';
import '../services/achievement_service.dart';
import '../services/app_settings_service.dart';

/// Provider for achievement service
final achievementServiceProvider = Provider<AchievementService>((ref) {
  final prefs = ref.watch(sharedPreferencesProvider);
  return AchievementService(prefs);
});

/// All achievements provider
final achievementsProvider = Provider<List<Achievement>>((ref) {
  return Achievements.all;
});

/// Unlocked achievements provider
final unlockedAchievementsProvider = Provider<List<Achievement>>((ref) {
  final service = ref.watch(achievementServiceProvider);
  return service.getUnlockedAchievements();
});

/// Achievement progress provider for specific achievement
final achievementProgressProvider =
    Provider.family<AchievementProgress, String>(
  (ref, achievementId) {
    final service = ref.watch(achievementServiceProvider);
    return service.getProgress(achievementId);
  },
);

/// Achievements by category provider
final achievementsByCategoryProvider =
    Provider<Map<AchievementCategory, List<Achievement>>>((ref) {
  final achievements = ref.watch(achievementsProvider);
  final Map<AchievementCategory, List<Achievement>> byCategory = {};

  for (final category in AchievementCategory.values) {
    byCategory[category] =
        achievements.where((a) => a.category == category).toList();
  }

  return byCategory;
});

/// Unlocked count by category provider
final unlockedCountByCategoryProvider =
    Provider<Map<AchievementCategory, int>>((ref) {
  final service = ref.watch(achievementServiceProvider);
  return service.getUnlockedCountByCategory();
});

/// Overall completion percentage provider
final completionPercentageProvider = Provider<double>((ref) {
  final service = ref.watch(achievementServiceProvider);
  return service.getCompletionPercentage();
});

/// Recently unlocked achievements provider (last 24 hours)
final recentlyUnlockedProvider = Provider<List<Achievement>>((ref) {
  final service = ref.watch(achievementServiceProvider);
  final unlocked = service.getUnlockedAchievements();
  final now = DateTime.now();

  return unlocked.where((achievement) {
    final progress = service.getProgress(achievement.id);
    if (progress.unlockedAt == null) return false;
    final difference = now.difference(progress.unlockedAt!);
    return difference.inHours < 24;
  }).toList();
});

/// Achievement notification state
class AchievementNotification {
  final Achievement achievement;
  final DateTime timestamp;

  const AchievementNotification({
    required this.achievement,
    required this.timestamp,
  });
}

/// Achievement notifier for showing unlock notifications
class AchievementNotifier extends StateNotifier<AchievementNotification?> {
  AchievementNotifier() : super(null);

  void showUnlock(Achievement achievement) {
    state = AchievementNotification(
      achievement: achievement,
      timestamp: DateTime.now(),
    );
  }

  void clear() {
    state = null;
  }
}

final achievementNotificationProvider =
    StateNotifierProvider<AchievementNotifier, AchievementNotification?>((ref) {
  return AchievementNotifier();
});

/// Helper functions to trigger achievement progress

/// Increment achievement progress and show notification if unlocked
Future<void> incrementAchievement(
  WidgetRef ref,
  String achievementId, {
  int amount = 1,
}) async {
  final service = ref.read(achievementServiceProvider);
  final unlocked =
      await service.incrementProgress(achievementId, amount: amount);

  if (unlocked) {
    final achievement = Achievements.getById(achievementId);
    if (achievement != null) {
      ref
          .read(achievementNotificationProvider.notifier)
          .showUnlock(achievement);
    }
  }
}
