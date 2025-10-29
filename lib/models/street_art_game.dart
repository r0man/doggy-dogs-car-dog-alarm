import 'dart:ui';

/// Street art tagging mini-game models
class StreetArtGameState {
  final bool isPlaying;
  final int score;
  final List<ArtStroke> strokes;
  final List<ArtChallenge> challenges;
  final int currentChallengeIndex;
  final double timeRemaining;

  const StreetArtGameState({
    this.isPlaying = false,
    this.score = 0,
    this.strokes = const [],
    this.challenges = const [],
    this.currentChallengeIndex = 0,
    this.timeRemaining = 30.0,
  });

  StreetArtGameState copyWith({
    bool? isPlaying,
    int? score,
    List<ArtStroke>? strokes,
    List<ArtChallenge>? challenges,
    int? currentChallengeIndex,
    double? timeRemaining,
  }) {
    return StreetArtGameState(
      isPlaying: isPlaying ?? this.isPlaying,
      score: score ?? this.score,
      strokes: strokes ?? this.strokes,
      challenges: challenges ?? this.challenges,
      currentChallengeIndex:
          currentChallengeIndex ?? this.currentChallengeIndex,
      timeRemaining: timeRemaining ?? this.timeRemaining,
    );
  }

  ArtChallenge? get currentChallenge {
    if (currentChallengeIndex < challenges.length) {
      return challenges[currentChallengeIndex];
    }
    return null;
  }

  bool get isComplete => currentChallengeIndex >= challenges.length;
}

/// A stroke/path drawn by the player
class ArtStroke {
  final List<Offset> points;
  final Color color;
  final double width;

  const ArtStroke({
    required this.points,
    required this.color,
    this.width = 4.0,
  });
}

/// Art challenge - player must draw something
class ArtChallenge {
  final String id;
  final String prompt;
  final String icon;
  final int points;
  final double timeLimit;
  final ArtStyle style;

  const ArtChallenge({
    required this.id,
    required this.prompt,
    required this.icon,
    required this.points,
    this.timeLimit = 30.0,
    this.style = ArtStyle.freeForm,
  });
}

/// Art style categories
enum ArtStyle {
  freeForm('Free Form', 'Express yourself'),
  stencil('Stencil', 'Clean lines and shapes'),
  wildstyle('Wildstyle', 'Complex and layered'),
  throwUp('Throw-Up', 'Quick and bold');

  final String displayName;
  final String description;

  const ArtStyle(this.displayName, this.description);
}

/// Predefined art challenges
class ArtChallenges {
  static const List<ArtChallenge> all = [
    ArtChallenge(
      id: 'tag_name',
      prompt: 'Tag your dog\'s name',
      icon: '✍️',
      points: 100,
      timeLimit: 30.0,
      style: ArtStyle.freeForm,
    ),
    ArtChallenge(
      id: 'guard_dog',
      prompt: 'Draw a guard dog',
      icon: '🐕',
      points: 150,
      timeLimit: 45.0,
      style: ArtStyle.stencil,
    ),
    ArtChallenge(
      id: 'city_skyline',
      prompt: 'Create a city skyline',
      icon: '🏙️',
      points: 200,
      timeLimit: 60.0,
      style: ArtStyle.wildstyle,
    ),
    ArtChallenge(
      id: 'alarm_symbol',
      prompt: 'Design an alarm symbol',
      icon: '🚨',
      points: 120,
      timeLimit: 30.0,
      style: ArtStyle.throwUp,
    ),
    ArtChallenge(
      id: 'urban_wildcard',
      prompt: 'Urban wildcard - surprise us!',
      icon: '🎨',
      points: 250,
      timeLimit: 60.0,
      style: ArtStyle.freeForm,
    ),
  ];
}

/// Spray paint colors
class SprayPaintColors {
  static const Color neonPink = Color(0xFFFF10F0);
  static const Color electricBlue = Color(0xFF00F5FF);
  static const Color toxicGreen = Color(0xFF39FF14);
  static const Color blazeOrange = Color(0xFFFF6600);
  static const Color violetPurple = Color(0xFF9D00FF);
  static const Color canaryYellow = Color(0xFFFFFF00);
  static const Color crimsonRed = Color(0xFFDC143C);
  static const Color silverChrome = Color(0xFFE8E8E8);
  static const Color deepBlack = Color(0xFF1a1a1a);

  static const List<Color> all = [
    neonPink,
    electricBlue,
    toxicGreen,
    blazeOrange,
    violetPurple,
    canaryYellow,
    crimsonRed,
    silverChrome,
    deepBlack,
  ];

  static String getColorName(Color color) {
    if (color == neonPink) return 'Neon Pink';
    if (color == electricBlue) return 'Electric Blue';
    if (color == toxicGreen) return 'Toxic Green';
    if (color == blazeOrange) return 'Blaze Orange';
    if (color == violetPurple) return 'Violet Purple';
    if (color == canaryYellow) return 'Canary Yellow';
    if (color == crimsonRed) return 'Crimson Red';
    if (color == silverChrome) return 'Silver Chrome';
    if (color == deepBlack) return 'Deep Black';
    return 'Unknown';
  }
}
