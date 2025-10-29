/// Urban neighborhood/district types
enum Neighborhood {
  downtown,
  artsQuarter,
  industrialZone,
  waterfront,
  uptown,
  outskirts,
}

extension NeighborhoodExtension on Neighborhood {
  String get displayName {
    switch (this) {
      case Neighborhood.downtown:
        return 'Downtown';
      case Neighborhood.artsQuarter:
        return 'Arts Quarter';
      case Neighborhood.industrialZone:
        return 'Industrial Zone';
      case Neighborhood.waterfront:
        return 'Waterfront';
      case Neighborhood.uptown:
        return 'Uptown';
      case Neighborhood.outskirts:
        return 'Outskirts';
    }
  }

  String get description {
    switch (this) {
      case Neighborhood.downtown:
        return 'Corporate center. Suits, skyscrapers, espresso machines.';
      case Neighborhood.artsQuarter:
        return 'Bohemian district. Galleries, coffee shops, street artists.';
      case Neighborhood.industrialZone:
        return 'Gritty warehouse district. Concrete and corrugated metal.';
      case Neighborhood.waterfront:
        return 'Docks and piers. Salt air, seagulls, shipping containers.';
      case Neighborhood.uptown:
        return 'Wealthy district. Boutiques, doormen, valet parking.';
      case Neighborhood.outskirts:
        return 'Suburban edge. Strip malls, chain stores, parking lots.';
    }
  }

  /// Visual style characteristics
  NeighborhoodStyle get style {
    switch (this) {
      case Neighborhood.downtown:
        return const NeighborhoodStyle(
          primaryColor: 0xFF2C3E50, // Dark blue-gray (corporate)
          accentColor: 0xFF3498DB, // Professional blue
          icon: '🏢',
          atmosphere: 'Busy, professional, coffee-fueled',
        );
      case Neighborhood.artsQuarter:
        return const NeighborhoodStyle(
          primaryColor: 0xFF9B59B6, // Purple (bohemian)
          accentColor: 0xFFE74C3C, // Artistic red
          icon: '🎨',
          atmosphere: 'Creative, laid-back, expressive',
        );
      case Neighborhood.industrialZone:
        return const NeighborhoodStyle(
          primaryColor: 0xFF34495E, // Dark gray (industrial)
          accentColor: 0xFFE85D27, // Rust orange
          icon: '🏭',
          atmosphere: 'Gritty, raw, no-nonsense',
        );
      case Neighborhood.waterfront:
        return const NeighborhoodStyle(
          primaryColor: 0xFF16A085, // Teal (water)
          accentColor: 0xFF00F5FF, // Cyan
          icon: '⚓',
          atmosphere: 'Salty, breezy, maritime',
        );
      case Neighborhood.uptown:
        return const NeighborhoodStyle(
          primaryColor: 0xFFF39C12, // Gold (luxury)
          accentColor: 0xFFE8E8E8, // White/silver
          icon: '💎',
          atmosphere: 'Polished, expensive, exclusive',
        );
      case Neighborhood.outskirts:
        return const NeighborhoodStyle(
          primaryColor: 0xFF7F8C8D, // Gray (suburban)
          accentColor: 0xFF95A5A6, // Light gray
          icon: '🏘️',
          atmosphere: 'Sprawling, familiar, mundane',
        );
    }
  }

  /// Threat level characteristics (affects alarm sensitivity)
  NeighborhoodSecurity get security {
    switch (this) {
      case Neighborhood.downtown:
        return const NeighborhoodSecurity(
          baseThreatLevel: 5,
          timeMultiplier: {
            TimeOfDay.day: 0.8,
            TimeOfDay.evening: 1.2,
            TimeOfDay.lateNight: 1.5,
          },
          description: 'Moderate crime. Higher at night.',
        );
      case Neighborhood.artsQuarter:
        return const NeighborhoodSecurity(
          baseThreatLevel: 4,
          timeMultiplier: {
            TimeOfDay.day: 0.7,
            TimeOfDay.evening: 1.0,
            TimeOfDay.lateNight: 1.3,
          },
          description: 'Generally safe. Eclectic crowd.',
        );
      case Neighborhood.industrialZone:
        return const NeighborhoodSecurity(
          baseThreatLevel: 7,
          timeMultiplier: {
            TimeOfDay.day: 1.0,
            TimeOfDay.evening: 1.3,
            TimeOfDay.lateNight: 1.8,
          },
          description: 'Sketchy. Watch your stuff.',
        );
      case Neighborhood.waterfront:
        return const NeighborhoodSecurity(
          baseThreatLevel: 6,
          timeMultiplier: {
            TimeOfDay.day: 0.9,
            TimeOfDay.evening: 1.2,
            TimeOfDay.lateNight: 1.6,
          },
          description: 'Rough area. Stay alert.',
        );
      case Neighborhood.uptown:
        return const NeighborhoodSecurity(
          baseThreatLevel: 2,
          timeMultiplier: {
            TimeOfDay.day: 0.5,
            TimeOfDay.evening: 0.8,
            TimeOfDay.lateNight: 1.2,
          },
          description: 'Very safe. Security everywhere.',
        );
      case Neighborhood.outskirts:
        return const NeighborhoodSecurity(
          baseThreatLevel: 3,
          timeMultiplier: {
            TimeOfDay.day: 0.6,
            TimeOfDay.evening: 1.0,
            TimeOfDay.lateNight: 1.4,
          },
          description: 'Pretty safe. Occasional trouble.',
        );
    }
  }

  /// Commentary lines for this neighborhood
  List<String> get commentaries {
    switch (this) {
      case Neighborhood.downtown:
        return [
          "Downtown. Everyone's in a hurry here.",
          "Corporate district. So many suits.",
          "The business center. Hope you're not shopping.",
          "Downtown parking? Bold. Expensive too, I bet.",
          "Corporate jungle. Watch for briefcase bandits.",
        ];
      case Neighborhood.artsQuarter:
        return [
          "Arts Quarter. I appreciate the vibe.",
          "Bohemian district. Very creative. Very... colorful.",
          "Artsy neighborhood. Bet there's good coffee nearby.",
          "Arts Quarter. Everyone's very expressive here.",
          "The creative district. Interesting crowd.",
        ];
      case Neighborhood.industrialZone:
        return [
          "Industrial Zone. This is... gritty.",
          "Warehouse district. Keep the doors locked.",
          "Industrial area. Not exactly scenic.",
          "Factory district. Definitely watch your back here.",
          "Industrial Zone. At least it's authentic urban.",
        ];
      case Neighborhood.waterfront:
        return [
          "Waterfront. I can smell the salt from here.",
          "Docks. Seagulls and shipping containers.",
          "Maritime district. Salty air, salty characters.",
          "Waterfront parking. Hope you like seagull presents.",
          "The docks. Rough but has character.",
        ];
      case Neighborhood.uptown:
        return [
          "Uptown. Fancy. Very fancy.",
          "Rich people district. Try not to ding any Bentleys.",
          "Uptown. Everything costs twice as much here.",
          "Wealthy neighborhood. I feel underdressed.",
          "Uptown parking. At least it's clean.",
        ];
      case Neighborhood.outskirts:
        return [
          "Outskirts. Suburban life in action.",
          "Edge of town. Strip malls as far as the eye can see.",
          "Outskirts. It's... functional.",
          "Suburban district. Very parking-lot-y.",
          "The outskirts. Not exciting, but safe.",
        ];
    }
  }
}

/// Visual style for a neighborhood
class NeighborhoodStyle {
  final int primaryColor;
  final int accentColor;
  final String icon;
  final String atmosphere;

  const NeighborhoodStyle({
    required this.primaryColor,
    required this.accentColor,
    required this.icon,
    required this.atmosphere,
  });
}

/// Security characteristics for a neighborhood
class NeighborhoodSecurity {
  final int baseThreatLevel; // 1-10
  final Map<TimeOfDay, double> timeMultiplier;
  final String description;

  const NeighborhoodSecurity({
    required this.baseThreatLevel,
    required this.timeMultiplier,
    required this.description,
  });

  /// Calculate current threat level based on time of day
  int getThreatLevel(TimeOfDay time) {
    final multiplier = timeMultiplier[time] ?? 1.0;
    return (baseThreatLevel * multiplier).round().clamp(1, 10);
  }
}

/// Time of day enum
enum TimeOfDay {
  day, // 6am - 6pm
  evening, // 6pm - 11pm
  lateNight, // 11pm - 6am
}

extension TimeOfDayExtension on TimeOfDay {
  String get displayName {
    switch (this) {
      case TimeOfDay.day:
        return 'Day';
      case TimeOfDay.evening:
        return 'Evening';
      case TimeOfDay.lateNight:
        return 'Late Night';
    }
  }

  /// Get current time of day from DateTime
  static TimeOfDay fromDateTime(DateTime dateTime) {
    final hour = dateTime.hour;
    if (hour >= 6 && hour < 18) {
      return TimeOfDay.day;
    } else if (hour >= 18 && hour < 23) {
      return TimeOfDay.evening;
    } else {
      return TimeOfDay.lateNight;
    }
  }
}

/// Location visit data for learning patterns
class LocationVisit {
  final Neighborhood neighborhood;
  final DateTime timestamp;
  final Duration duration;

  const LocationVisit({
    required this.neighborhood,
    required this.timestamp,
    required this.duration,
  });

  Map<String, dynamic> toJson() {
    return {
      'neighborhood': neighborhood.name,
      'timestamp': timestamp.toIso8601String(),
      'duration': duration.inSeconds,
    };
  }

  factory LocationVisit.fromJson(Map<String, dynamic> json) {
    return LocationVisit(
      neighborhood: Neighborhood.values.firstWhere(
        (e) => e.name == json['neighborhood'],
      ),
      timestamp: DateTime.parse(json['timestamp'] as String),
      duration: Duration(seconds: json['duration'] as int),
    );
  }
}
