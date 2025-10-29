/// Achievement categories
enum AchievementCategory {
  security,
  social,
  exploration,
  personal,
  reputation,
}

/// Urban-themed achievement
class Achievement {
  final String id;
  final String name;
  final String description;
  final AchievementCategory category;
  final int requirement;
  final String icon;
  final String rewardDescription;

  const Achievement({
    required this.id,
    required this.name,
    required this.description,
    required this.category,
    required this.requirement,
    required this.icon,
    required this.rewardDescription,
  });
}

/// Achievement progress tracking
class AchievementProgress {
  final String achievementId;
  final int currentProgress;
  final DateTime? unlockedAt;

  const AchievementProgress({
    required this.achievementId,
    required this.currentProgress,
    this.unlockedAt,
  });

  bool get isUnlocked => unlockedAt != null;

  double getProgress(int requirement) {
    if (isUnlocked) return 1.0;
    return (currentProgress / requirement).clamp(0.0, 1.0);
  }

  AchievementProgress copyWith({
    int? currentProgress,
    DateTime? unlockedAt,
  }) {
    return AchievementProgress(
      achievementId: achievementId,
      currentProgress: currentProgress ?? this.currentProgress,
      unlockedAt: unlockedAt ?? this.unlockedAt,
    );
  }

  Map<String, dynamic> toJson() {
    return {
      'achievementId': achievementId,
      'currentProgress': currentProgress,
      'unlockedAt': unlockedAt?.toIso8601String(),
    };
  }

  factory AchievementProgress.fromJson(Map<String, dynamic> json) {
    return AchievementProgress(
      achievementId: json['achievementId'] as String,
      currentProgress: json['currentProgress'] as int,
      unlockedAt: json['unlockedAt'] != null
          ? DateTime.parse(json['unlockedAt'] as String)
          : null,
    );
  }
}

/// All urban-themed achievements
class Achievements {
  static const List<Achievement> all = [
    // SECURITY ACHIEVEMENTS
    Achievement(
      id: 'bark_and_disorderly',
      name: 'Bark & Disorderly',
      description: 'Activate alarm 100 times. You take this seriously.',
      category: AchievementCategory.security,
      requirement: 100,
      icon: '🚨',
      rewardDescription: 'Unlocks "Veteran Guard" badge',
    ),
    Achievement(
      id: 'first_watch',
      name: 'First Watch',
      description:
          'Successfully scare off your first threat. Welcome to the job.',
      category: AchievementCategory.security,
      requirement: 1,
      icon: '👀',
      rewardDescription: 'Unlocks basic guard dog badge',
    ),
    Achievement(
      id: 'false_alarm_fred',
      name: 'False Alarm Fred',
      description: 'Trigger 50 false alarms. Maybe dial it back a bit?',
      category: AchievementCategory.security,
      requirement: 50,
      icon: '😅',
      rewardDescription: 'Unlocks "Overzealous" title',
    ),
    Achievement(
      id: 'meter_maids_nightmare',
      name: "Meter Maid's Worst Nightmare",
      description:
          'Save owner from 10 parking tickets. Doing the lord\'s work.',
      category: AchievementCategory.security,
      requirement: 10,
      icon: '🎫',
      rewardDescription: 'Unlocks parking enforcement badge',
    ),
    Achievement(
      id: 'night_shift',
      name: 'Night Shift',
      description: 'Guard 50 times after midnight. The streets are yours.',
      category: AchievementCategory.security,
      requirement: 50,
      icon: '🌙',
      rewardDescription: 'Unlocks night vision goggles (cosmetic)',
    ),

    // EXPLORATION ACHIEVEMENTS
    Achievement(
      id: 'concrete_jungle_survivor',
      name: 'Concrete Jungle Survivor',
      description: 'Park in all 6 neighborhoods. You get around.',
      category: AchievementCategory.exploration,
      requirement: 6,
      icon: '🗺️',
      rewardDescription: 'Unlocks city map badge',
    ),
    Achievement(
      id: 'street_cred',
      name: 'Street Cred',
      description: 'Max out Downtown reputation. Corporate respect earned.',
      category: AchievementCategory.exploration,
      requirement: 100,
      icon: '💼',
      rewardDescription: 'Unlocks business casual collar',
    ),
    Achievement(
      id: 'arts_district_regular',
      name: 'Arts District Regular',
      description: 'Visit Arts Quarter 25 times. They know you there.',
      category: AchievementCategory.exploration,
      requirement: 25,
      icon: '🎨',
      rewardDescription: 'Unlocks artsy bandana',
    ),
    Achievement(
      id: 'waterfront_wanderer',
      name: 'Waterfront Wanderer',
      description: 'Patrol the docks 20 times. Salt in your fur.',
      category: AchievementCategory.exploration,
      requirement: 20,
      icon: '⚓',
      rewardDescription: 'Unlocks sailor collar',
    ),
    Achievement(
      id: 'uptown_dog',
      name: 'Uptown Dog',
      description: 'Guard in Uptown 15 times. Living fancy.',
      category: AchievementCategory.exploration,
      requirement: 15,
      icon: '💎',
      rewardDescription: 'Unlocks luxury collar',
    ),

    // PERSONAL ACHIEVEMENTS
    Achievement(
      id: 'reformed',
      name: 'Reformed',
      description: 'Perfect care streak for 30 days. Character development.',
      category: AchievementCategory.personal,
      requirement: 30,
      icon: '✨',
      rewardDescription: 'Unlocks halo accessory',
    ),
    Achievement(
      id: 'well_fed',
      name: 'Well Fed',
      description: 'Fed 100 times. Your owner actually remembers you exist.',
      category: AchievementCategory.personal,
      requirement: 100,
      icon: '🍖',
      rewardDescription: 'Unlocks gourmet food bowl',
    ),
    Achievement(
      id: 'playful_spirit',
      name: 'Playful Spirit',
      description: 'Play 75 times. All work and no play, you know?',
      category: AchievementCategory.personal,
      requirement: 75,
      icon: '🎾',
      rewardDescription: 'Unlocks favorite toy',
    ),
    Achievement(
      id: 'level_cap',
      name: 'Top Dog',
      description: 'Reach level 50. Maximum experience achieved.',
      category: AchievementCategory.personal,
      requirement: 50,
      icon: '⭐',
      rewardDescription: 'Unlocks legendary collar',
    ),

    // SOCIAL ACHIEVEMENTS
    Achievement(
      id: 'chatterbox',
      name: 'Chatterbox',
      description: 'Trigger 200 dialogue responses. You got opinions.',
      category: AchievementCategory.social,
      requirement: 200,
      icon: '💬',
      rewardDescription: 'Unlocks megaphone accessory',
    ),
    Achievement(
      id: 'known_around_town',
      name: 'Known Around Town',
      description: 'Visit each neighborhood 10 times. Local celebrity.',
      category: AchievementCategory.social,
      requirement: 60,
      icon: '🌟',
      rewardDescription: 'Unlocks VIP badge',
    ),

    // REPUTATION ACHIEVEMENTS
    Achievement(
      id: 'gentrification_witness',
      name: 'Gentrification Witness',
      description:
          'See all neighborhoods in different times of day. You\'ve seen changes.',
      category: AchievementCategory.reputation,
      requirement: 18,
      icon: '🏙️',
      rewardDescription: 'Unlocks historian badge',
    ),
    Achievement(
      id: 'urban_legend',
      name: 'Urban Legend',
      description: 'Complete all other achievements. The streets remember you.',
      category: AchievementCategory.reputation,
      requirement: 1,
      icon: '👑',
      rewardDescription: 'Unlocks legendary status',
    ),
  ];

  /// Get achievement by ID
  static Achievement? getById(String id) {
    try {
      return all.firstWhere((a) => a.id == id);
    } catch (e) {
      return null;
    }
  }

  /// Get achievements by category
  static List<Achievement> getByCategory(AchievementCategory category) {
    return all.where((a) => a.category == category).toList();
  }
}
