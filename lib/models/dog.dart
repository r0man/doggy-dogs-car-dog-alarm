/// Represents a virtual guard dog with personality and stats
class Dog {
  final String id;
  final String name;
  final DogBreed breed;
  final int level;
  final int experience;
  final DogStats stats;
  final DogPersonality personality;
  final DogMood currentMood;
  final DateTime createdAt;
  final DateTime lastInteraction;

  const Dog({
    required this.id,
    required this.name,
    required this.breed,
    this.level = 1,
    this.experience = 0,
    required this.stats,
    required this.personality,
    this.currentMood = DogMood.content,
    required this.createdAt,
    required this.lastInteraction,
  });

  Dog copyWith({
    String? id,
    String? name,
    DogBreed? breed,
    int? level,
    int? experience,
    DogStats? stats,
    DogPersonality? personality,
    DogMood? currentMood,
    DateTime? createdAt,
    DateTime? lastInteraction,
  }) {
    return Dog(
      id: id ?? this.id,
      name: name ?? this.name,
      breed: breed ?? this.breed,
      level: level ?? this.level,
      experience: experience ?? this.experience,
      stats: stats ?? this.stats,
      personality: personality ?? this.personality,
      currentMood: currentMood ?? this.currentMood,
      createdAt: createdAt ?? this.createdAt,
      lastInteraction: lastInteraction ?? this.lastInteraction,
    );
  }

  /// Calculate XP needed for next level
  int get xpForNextLevel => level * 100;

  /// Calculate XP progress percentage
  double get xpProgress => experience / xpForNextLevel;

  /// Check if dog is neglected
  bool get isNeglected {
    final hoursSinceInteraction =
        DateTime.now().difference(lastInteraction).inHours;
    return hoursSinceInteraction > 24 ||
           stats.hunger < 30 ||
           stats.happiness < 30;
  }

  /// Get effectiveness rating (0-100) based on stats
  int get effectiveness {
    if (isNeglected) return 30;
    return ((stats.hunger + stats.happiness + stats.energy) / 3).round();
  }

  /// Convert to JSON
  Map<String, dynamic> toJson() {
    return {
      'id': id,
      'name': name,
      'breed': breed.name,
      'level': level,
      'experience': experience,
      'stats': stats.toJson(),
      'personality': personality.toJson(),
      'currentMood': currentMood.name,
      'createdAt': createdAt.toIso8601String(),
      'lastInteraction': lastInteraction.toIso8601String(),
    };
  }

  /// Create from JSON
  factory Dog.fromJson(Map<String, dynamic> json) {
    return Dog(
      id: json['id'] as String,
      name: json['name'] as String,
      breed: DogBreed.values.firstWhere((e) => e.name == json['breed']),
      level: json['level'] as int,
      experience: json['experience'] as int,
      stats: DogStats.fromJson(json['stats'] as Map<String, dynamic>),
      personality: DogPersonality.fromJson(json['personality'] as Map<String, dynamic>),
      currentMood: DogMood.values.firstWhere((e) => e.name == json['currentMood']),
      createdAt: DateTime.parse(json['createdAt'] as String),
      lastInteraction: DateTime.parse(json['lastInteraction'] as String),
    );
  }
}

/// Dog breed with unique characteristics
enum DogBreed {
  germanShepherd,
  rottweiler,
  doberman,
  bulldog,
  pitbull,
  husky,
  beagle,
}

extension DogBreedExtension on DogBreed {
  String get displayName {
    switch (this) {
      case DogBreed.germanShepherd:
        return 'German Shepherd';
      case DogBreed.rottweiler:
        return 'Rottweiler';
      case DogBreed.doberman:
        return 'Doberman';
      case DogBreed.bulldog:
        return 'Bulldog';
      case DogBreed.pitbull:
        return 'Pitbull';
      case DogBreed.husky:
        return 'Husky';
      case DogBreed.beagle:
        return 'Beagle';
    }
  }

  String get assetPath {
    switch (this) {
      case DogBreed.germanShepherd:
        return 'assets/dogs/german_shepherd.svg';
      case DogBreed.rottweiler:
        return 'assets/dogs/rottweiler.svg';
      case DogBreed.doberman:
        return 'assets/dogs/doberman.svg';
      case DogBreed.bulldog:
        return 'assets/dogs/bulldog.svg';
      case DogBreed.pitbull:
        return 'assets/dogs/pitbull.svg';
      case DogBreed.husky:
        return 'assets/dogs/husky.svg';
      case DogBreed.beagle:
        return 'assets/dogs/beagle.svg';
    }
  }

  String get description {
    switch (this) {
      case DogBreed.germanShepherd:
        return 'Loyal and alert with characteristic tan and black coloring';
      case DogBreed.rottweiler:
        return 'Strong protector with distinctive markings';
      case DogBreed.doberman:
        return 'Sleek and elegant with alert expression';
      case DogBreed.bulldog:
        return 'Stocky and determined with adorable wrinkles';
      case DogBreed.pitbull:
        return 'Muscular and friendly with a big smile';
      case DogBreed.husky:
        return 'Striking blue eyes and fluffy appearance';
      case DogBreed.beagle:
        return 'Sweet and curious with tri-color coat';
    }
  }
}

/// Dog's daily needs and stats
class DogStats {
  final int hunger; // 0-100
  final int happiness; // 0-100
  final int energy; // 0-100
  final int loyalty; // 0-100

  const DogStats({
    required this.hunger,
    required this.happiness,
    required this.energy,
    required this.loyalty,
  });

  DogStats copyWith({
    int? hunger,
    int? happiness,
    int? energy,
    int? loyalty,
  }) {
    return DogStats(
      hunger: hunger ?? this.hunger,
      happiness: happiness ?? this.happiness,
      energy: energy ?? this.energy,
      loyalty: loyalty ?? this.loyalty,
    );
  }

  /// Decrease stats over time
  DogStats decay({int hungerDecay = 5, int energyDecay = 3, int happinessDecay = 2}) {
    return DogStats(
      hunger: (hunger - hungerDecay).clamp(0, 100),
      happiness: (happiness - happinessDecay).clamp(0, 100),
      energy: (energy - energyDecay).clamp(0, 100),
      loyalty: loyalty,
    );
  }

  /// Convert to JSON
  Map<String, dynamic> toJson() {
    return {
      'hunger': hunger,
      'happiness': happiness,
      'energy': energy,
      'loyalty': loyalty,
    };
  }

  /// Create from JSON
  factory DogStats.fromJson(Map<String, dynamic> json) {
    return DogStats(
      hunger: json['hunger'] as int,
      happiness: json['happiness'] as int,
      energy: json['energy'] as int,
      loyalty: json['loyalty'] as int,
    );
  }
}

/// Dog personality traits
class DogPersonality {
  final bool brave;
  final bool nervous;
  final bool playful;
  final bool lazy;
  final bool energetic;
  final bool protective;

  const DogPersonality({
    this.brave = false,
    this.nervous = false,
    this.playful = false,
    this.lazy = false,
    this.energetic = false,
    this.protective = false,
  });

  /// Convert to JSON
  Map<String, dynamic> toJson() {
    return {
      'brave': brave,
      'nervous': nervous,
      'playful': playful,
      'lazy': lazy,
      'energetic': energetic,
      'protective': protective,
    };
  }

  /// Create from JSON
  factory DogPersonality.fromJson(Map<String, dynamic> json) {
    return DogPersonality(
      brave: json['brave'] as bool? ?? false,
      nervous: json['nervous'] as bool? ?? false,
      playful: json['playful'] as bool? ?? false,
      lazy: json['lazy'] as bool? ?? false,
      energetic: json['energetic'] as bool? ?? false,
      protective: json['protective'] as bool? ?? false,
    );
  }
}

/// Dog's current emotional state
enum DogMood {
  happy,
  content,
  alert,
  worried,
  sad,
  excited,
  sleeping,
  grumpy,
}

extension DogMoodExtension on DogMood {
  String get emoji {
    switch (this) {
      case DogMood.happy:
        return '😊';
      case DogMood.content:
        return '😌';
      case DogMood.alert:
        return '👀';
      case DogMood.worried:
        return '😟';
      case DogMood.sad:
        return '😢';
      case DogMood.excited:
        return '🤩';
      case DogMood.sleeping:
        return '😴';
      case DogMood.grumpy:
        return '😠';
    }
  }

  String get description {
    switch (this) {
      case DogMood.happy:
        return 'Your dog is happy!';
      case DogMood.content:
        return 'Your dog is content';
      case DogMood.alert:
        return 'Your dog is alert';
      case DogMood.worried:
        return 'Your dog seems worried';
      case DogMood.sad:
        return 'Your dog is sad';
      case DogMood.excited:
        return 'Your dog is excited!';
      case DogMood.sleeping:
        return 'Your dog is sleeping';
      case DogMood.grumpy:
        return 'Your dog is grumpy';
    }
  }
}
