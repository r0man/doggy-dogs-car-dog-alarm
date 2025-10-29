import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import '../models/achievement.dart';
import '../providers/achievement_provider.dart';
import '../theme/urban_colors.dart';
import '../theme/comic_decorations.dart';

/// Screen displaying all achievements and progress
class AchievementsScreen extends ConsumerWidget {
  const AchievementsScreen({super.key});

  @override
  Widget build(BuildContext context, WidgetRef ref) {
    final achievementsByCategory = ref.watch(achievementsByCategoryProvider);
    final unlockedCounts = ref.watch(unlockedCountByCategoryProvider);
    final completionPercentage = ref.watch(completionPercentageProvider);

    return Scaffold(
      appBar: AppBar(
        title: const Text('Street Cred'),
        elevation: 0,
        actions: [
          // Overall completion badge
          Padding(
            padding: const EdgeInsets.all(8.0),
            child: Center(
              child: Container(
                padding:
                    const EdgeInsets.symmetric(horizontal: 12, vertical: 6),
                decoration: BoxDecoration(
                  color: UrbanColors.neonCyan,
                  borderRadius: BorderRadius.circular(16),
                  border: Border.all(color: UrbanColors.comicBlack, width: 2),
                  boxShadow: [ComicDecorations.dropShadow],
                ),
                child: Text(
                  '${(completionPercentage * 100).toStringAsFixed(0)}%',
                  style: Theme.of(context).textTheme.labelLarge?.copyWith(
                        color: UrbanColors.comicBlack,
                        fontWeight: FontWeight.bold,
                      ),
                ),
              ),
            ),
          ),
        ],
      ),
      body: SafeArea(
        child: CustomScrollView(
          slivers: [
            // Header section
            SliverToFadingAppBar(
              child: _buildHeader(context, ref, completionPercentage),
            ),

            // Achievements by category
            for (final category in AchievementCategory.values) ...[
              _buildCategoryHeader(
                context,
                category,
                achievementsByCategory[category] ?? [],
                unlockedCounts[category] ?? 0,
              ),
              _buildAchievementList(
                ref,
                achievementsByCategory[category] ?? [],
              ),
            ],
          ],
        ),
      ),
    );
  }

  Widget _buildHeader(BuildContext context, WidgetRef ref, double completion) {
    final recentlyUnlocked = ref.watch(recentlyUnlockedProvider);
    final totalAchievements = Achievements.all.length;
    final unlockedCount = ref.watch(unlockedAchievementsProvider).length;

    return Container(
      padding: const EdgeInsets.all(16),
      color: UrbanColors.concreteGray,
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.stretch,
        children: [
          Text(
            'Your Street Credentials',
            style: Theme.of(context).textTheme.titleLarge?.copyWith(
                  color: UrbanColors.neonCyan,
                ),
          ),
          const SizedBox(height: 8),
          Text(
            '$unlockedCount / $totalAchievements badges earned',
            style: Theme.of(context).textTheme.bodyMedium,
          ),
          const SizedBox(height: 12),

          // Progress bar
          ClipRRect(
            borderRadius: BorderRadius.circular(8),
            child: Stack(
              children: [
                Container(
                  height: 24,
                  decoration: BoxDecoration(
                    color: UrbanColors.asphalt,
                    border: Border.all(color: UrbanColors.comicBlack, width: 2),
                    borderRadius: BorderRadius.circular(8),
                  ),
                ),
                FractionallySizedBox(
                  widthFactor: completion,
                  child: Container(
                    height: 24,
                    decoration: BoxDecoration(
                      gradient: const LinearGradient(
                        colors: [
                          UrbanColors.neonCyan,
                          UrbanColors.neonYellow,
                        ],
                      ),
                      border:
                          Border.all(color: UrbanColors.comicBlack, width: 2),
                      borderRadius: BorderRadius.circular(8),
                    ),
                  ),
                ),
              ],
            ),
          ),

          // Recently unlocked
          if (recentlyUnlocked.isNotEmpty) ...[
            const SizedBox(height: 16),
            Container(
              padding: const EdgeInsets.all(12),
              decoration: BoxDecoration(
                color: UrbanColors.neonYellow.withOpacity(0.1),
                border: Border.all(color: UrbanColors.neonYellow, width: 2),
                borderRadius: BorderRadius.circular(8),
              ),
              child: Row(
                children: [
                  const Text('🔥', style: TextStyle(fontSize: 24)),
                  const SizedBox(width: 12),
                  Expanded(
                    child: Column(
                      crossAxisAlignment: CrossAxisAlignment.start,
                      children: [
                        Text(
                          'Recently Earned',
                          style:
                              Theme.of(context).textTheme.labelSmall?.copyWith(
                                    color: UrbanColors.neonYellow,
                                    fontWeight: FontWeight.bold,
                                  ),
                        ),
                        Text(
                          recentlyUnlocked.map((a) => a.name).join(', '),
                          style: Theme.of(context).textTheme.bodySmall,
                          maxLines: 2,
                          overflow: TextOverflow.ellipsis,
                        ),
                      ],
                    ),
                  ),
                ],
              ),
            ),
          ],
        ],
      ),
    );
  }

  Widget _buildCategoryHeader(
    BuildContext context,
    AchievementCategory category,
    List<Achievement> achievements,
    int unlockedCount,
  ) {
    return SliverToBoxAdapter(
      child: Container(
        margin: const EdgeInsets.fromLTRB(16, 16, 16, 8),
        padding: const EdgeInsets.all(12),
        decoration: BoxDecoration(
          color: Color(category.color).withOpacity(0.2),
          border: Border.all(color: Color(category.color), width: 2),
          borderRadius: BorderRadius.circular(8),
        ),
        child: Row(
          children: [
            Text(
              category.icon,
              style: const TextStyle(fontSize: 24),
            ),
            const SizedBox(width: 12),
            Expanded(
              child: Column(
                crossAxisAlignment: CrossAxisAlignment.start,
                children: [
                  Text(
                    category.displayName,
                    style: Theme.of(context).textTheme.titleMedium?.copyWith(
                          color: Color(category.color),
                          fontWeight: FontWeight.bold,
                        ),
                  ),
                  Text(
                    '$unlockedCount / ${achievements.length} unlocked',
                    style: Theme.of(context).textTheme.bodySmall?.copyWith(
                          color: UrbanColors.fog,
                        ),
                  ),
                ],
              ),
            ),
          ],
        ),
      ),
    );
  }

  Widget _buildAchievementList(WidgetRef ref, List<Achievement> achievements) {
    return SliverPadding(
      padding: const EdgeInsets.symmetric(horizontal: 16),
      sliver: SliverList(
        delegate: SliverChildBuilderDelegate(
          (context, index) {
            final achievement = achievements[index];
            final progress =
                ref.watch(achievementProgressProvider(achievement.id));

            return _AchievementTile(
              achievement: achievement,
              progress: progress,
            );
          },
          childCount: achievements.length,
        ),
      ),
    );
  }
}

/// Individual achievement tile
class _AchievementTile extends StatelessWidget {
  final Achievement achievement;
  final AchievementProgress progress;

  const _AchievementTile({
    required this.achievement,
    required this.progress,
  });

  @override
  Widget build(BuildContext context) {
    final isUnlocked = progress.isUnlocked;
    final progressPercent =
        progress.progressPercentage(achievement.requirement);

    return Card(
      margin: const EdgeInsets.only(bottom: 12),
      color: isUnlocked
          ? Color(achievement.category.color).withOpacity(0.1)
          : UrbanColors.asphalt.withOpacity(0.3),
      child: Padding(
        padding: const EdgeInsets.all(16),
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            Row(
              children: [
                // Icon
                Container(
                  width: 48,
                  height: 48,
                  decoration: BoxDecoration(
                    color: isUnlocked
                        ? Color(achievement.category.color)
                        : UrbanColors.asphalt,
                    borderRadius: BorderRadius.circular(8),
                    border: Border.all(color: UrbanColors.comicBlack, width: 2),
                  ),
                  child: Center(
                    child: Text(
                      achievement.icon,
                      style: TextStyle(
                        fontSize: 24,
                        color: isUnlocked ? null : UrbanColors.fog,
                      ),
                    ),
                  ),
                ),
                const SizedBox(width: 12),

                // Name and description
                Expanded(
                  child: Column(
                    crossAxisAlignment: CrossAxisAlignment.start,
                    children: [
                      Row(
                        children: [
                          Expanded(
                            child: Text(
                              achievement.name,
                              style: Theme.of(context)
                                  .textTheme
                                  .titleSmall
                                  ?.copyWith(
                                    color: isUnlocked
                                        ? Color(achievement.category.color)
                                        : UrbanColors.fog,
                                    fontWeight: FontWeight.bold,
                                  ),
                            ),
                          ),
                          if (isUnlocked)
                            Container(
                              padding: const EdgeInsets.symmetric(
                                horizontal: 8,
                                vertical: 2,
                              ),
                              decoration: BoxDecoration(
                                color: UrbanColors.successGreen,
                                borderRadius: BorderRadius.circular(4),
                                border: Border.all(
                                  color: UrbanColors.comicBlack,
                                  width: 2,
                                ),
                              ),
                              child: Text(
                                'UNLOCKED',
                                style: Theme.of(context)
                                    .textTheme
                                    .labelSmall
                                    ?.copyWith(
                                      color: UrbanColors.comicBlack,
                                      fontWeight: FontWeight.bold,
                                    ),
                              ),
                            ),
                        ],
                      ),
                      const SizedBox(height: 4),
                      Text(
                        achievement.description,
                        style: Theme.of(context).textTheme.bodySmall?.copyWith(
                              color: isUnlocked ? null : UrbanColors.fog,
                            ),
                      ),
                    ],
                  ),
                ),
              ],
            ),

            // Progress bar (if not unlocked)
            if (!isUnlocked) ...[
              const SizedBox(height: 12),
              Column(
                crossAxisAlignment: CrossAxisAlignment.stretch,
                children: [
                  Row(
                    mainAxisAlignment: MainAxisAlignment.spaceBetween,
                    children: [
                      Text(
                        'Progress: ${progress.currentProgress} / ${achievement.requirement}',
                        style: Theme.of(context).textTheme.bodySmall?.copyWith(
                              color: UrbanColors.fog,
                            ),
                      ),
                      Text(
                        '${(progressPercent * 100).toStringAsFixed(0)}%',
                        style: Theme.of(context).textTheme.bodySmall?.copyWith(
                              color: UrbanColors.neonCyan,
                              fontWeight: FontWeight.bold,
                            ),
                      ),
                    ],
                  ),
                  const SizedBox(height: 6),
                  ClipRRect(
                    borderRadius: BorderRadius.circular(4),
                    child: Stack(
                      children: [
                        Container(
                          height: 8,
                          decoration: BoxDecoration(
                            color: UrbanColors.asphalt,
                            border: Border.all(
                                color: UrbanColors.comicBlack, width: 1),
                            borderRadius: BorderRadius.circular(4),
                          ),
                        ),
                        FractionallySizedBox(
                          widthFactor: progressPercent,
                          child: Container(
                            height: 8,
                            decoration: BoxDecoration(
                              color: Color(achievement.category.color),
                              border: Border.all(
                                  color: UrbanColors.comicBlack, width: 1),
                              borderRadius: BorderRadius.circular(4),
                            ),
                          ),
                        ),
                      ],
                    ),
                  ),
                ],
              ),
            ],

            // Reward (if unlocked)
            if (isUnlocked) ...[
              const SizedBox(height: 12),
              Container(
                padding: const EdgeInsets.all(8),
                decoration: BoxDecoration(
                  color: Color(achievement.category.color).withOpacity(0.2),
                  borderRadius: BorderRadius.circular(4),
                  border: Border.all(
                      color: Color(achievement.category.color), width: 1),
                ),
                child: Row(
                  children: [
                    const Icon(Icons.stars,
                        size: 16, color: UrbanColors.neonYellow),
                    const SizedBox(width: 8),
                    Expanded(
                      child: Text(
                        achievement.rewardDescription,
                        style: Theme.of(context).textTheme.bodySmall?.copyWith(
                              fontStyle: FontStyle.italic,
                            ),
                      ),
                    ),
                  ],
                ),
              ),
            ],

            // Unlocked date
            if (isUnlocked && progress.unlockedAt != null) ...[
              const SizedBox(height: 8),
              Text(
                'Earned ${_formatDate(progress.unlockedAt!)}',
                style: Theme.of(context).textTheme.bodySmall?.copyWith(
                      color: UrbanColors.fog,
                      fontStyle: FontStyle.italic,
                    ),
              ),
            ],
          ],
        ),
      ),
    );
  }

  String _formatDate(DateTime date) {
    final now = DateTime.now();
    final difference = now.difference(date);

    if (difference.inDays == 0) {
      return 'today';
    } else if (difference.inDays == 1) {
      return 'yesterday';
    } else if (difference.inDays < 7) {
      return '${difference.inDays} days ago';
    } else {
      return '${date.month}/${date.day}/${date.year}';
    }
  }
}

/// Sliver that fades into the app bar
class SliverToFadingAppBar extends StatelessWidget {
  final Widget child;

  const SliverToFadingAppBar({super.key, required this.child});

  @override
  Widget build(BuildContext context) {
    return SliverToBoxAdapter(child: child);
  }
}
