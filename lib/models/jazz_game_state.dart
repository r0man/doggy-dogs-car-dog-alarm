/// State for jazz rhythm mini-game
class JazzGameState {
  final bool isPlaying;
  final int score;
  final int combo;
  final int maxCombo;
  final List<JazzNote> notes;
  final double gameTime;

  const JazzGameState({
    this.isPlaying = false,
    this.score = 0,
    this.combo = 0,
    this.maxCombo = 0,
    this.notes = const [],
    this.gameTime = 0.0,
  });

  JazzGameState copyWith({
    bool? isPlaying,
    int? score,
    int? combo,
    int? maxCombo,
    List<JazzNote>? notes,
    double? gameTime,
  }) {
    return JazzGameState(
      isPlaying: isPlaying ?? this.isPlaying,
      score: score ?? this.score,
      combo: combo ?? this.combo,
      maxCombo: maxCombo ?? this.maxCombo,
      notes: notes ?? this.notes,
      gameTime: gameTime ?? this.gameTime,
    );
  }
}

/// Individual note in the jazz rhythm game
class JazzNote {
  final int lane; // 0-3 (representing 4 lanes)
  final double spawnTime;
  final double targetTime;
  final bool hit;
  final JazzInstrument instrument;

  const JazzNote({
    required this.lane,
    required this.spawnTime,
    required this.targetTime,
    this.hit = false,
    this.instrument = JazzInstrument.snare,
  });

  JazzNote copyWith({
    int? lane,
    double? spawnTime,
    double? targetTime,
    bool? hit,
    JazzInstrument? instrument,
  }) {
    return JazzNote(
      lane: lane ?? this.lane,
      spawnTime: spawnTime ?? this.spawnTime,
      targetTime: targetTime ?? this.targetTime,
      hit: hit ?? this.hit,
      instrument: instrument ?? this.instrument,
    );
  }

  /// Check if note is in hit window
  bool isInHitWindow(double currentTime) {
    final diff = (currentTime - targetTime).abs();
    return diff < 0.2; // 200ms hit window
  }

  /// Get hit accuracy
  JazzHitAccuracy getAccuracy(double currentTime) {
    final diff = (currentTime - targetTime).abs();
    if (diff < 0.05) return JazzHitAccuracy.perfect;
    if (diff < 0.1) return JazzHitAccuracy.great;
    if (diff < 0.15) return JazzHitAccuracy.good;
    return JazzHitAccuracy.miss;
  }
}

/// Jazz instruments
enum JazzInstrument {
  snare('🥁', 'Snare'),
  cymbal('🎵', 'Cymbal'),
  bass('🎸', 'Bass'),
  piano('🎹', 'Piano');

  final String icon;
  final String name;

  const JazzInstrument(this.icon, this.name);
}

/// Hit accuracy levels
enum JazzHitAccuracy {
  perfect(100, 'PERFECT!'),
  great(75, 'Great'),
  good(50, 'Good'),
  miss(0, 'Miss');

  final int points;
  final String displayName;

  const JazzHitAccuracy(this.points, this.displayName);
}

/// Jazz song difficulty
enum JazzDifficulty {
  easy('Easy', 'Smooth jazz. Nice and easy.', 60),
  medium('Medium', 'Bebop. Pick up the pace.', 90),
  hard('Hard', 'Free jazz. Chaos and skill.', 120);

  final String displayName;
  final String description;
  final int bpm;

  const JazzDifficulty(this.displayName, this.description, this.bpm);
}
