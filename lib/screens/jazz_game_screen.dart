import 'dart:async';
import 'dart:math';
import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import '../models/jazz_game_state.dart';
import '../theme/urban_colors.dart';
import '../providers/achievement_provider.dart';

/// Jazz rhythm mini-game screen
class JazzGameScreen extends ConsumerStatefulWidget {
  final JazzDifficulty difficulty;

  const JazzGameScreen({
    super.key,
    this.difficulty = JazzDifficulty.easy,
  });

  @override
  ConsumerState<JazzGameScreen> createState() => _JazzGameScreenState();
}

class _JazzGameScreenState extends ConsumerState<JazzGameScreen> {
  late JazzGameState _state;
  Timer? _gameTimer;
  Timer? _noteSpawner;
  final Random _random = Random();
  final int _numLanes = 4;
  JazzHitAccuracy? _lastHit;

  @override
  void initState() {
    super.initState();
    _state = const JazzGameState();
  }

  @override
  void dispose() {
    _gameTimer?.cancel();
    _noteSpawner?.cancel();
    super.dispose();
  }

  void _startGame() {
    setState(() {
      _state = const JazzGameState(isPlaying: true);
    });

    // Game update loop (60 FPS)
    _gameTimer = Timer.periodic(const Duration(milliseconds: 16), (timer) {
      if (!mounted) {
        timer.cancel();
        return;
      }

      setState(() {
        _state = _state.copyWith(
          gameTime: _state.gameTime + 0.016,
          notes: _state.notes.where((note) {
            // Remove notes that are past the target
            return _state.gameTime < note.targetTime + 0.5;
          }).toList(),
        );
      });
    });

    // Spawn notes based on difficulty
    final spawnInterval = 60.0 / widget.difficulty.bpm; // Seconds per beat
    _noteSpawner = Timer.periodic(
        Duration(milliseconds: (spawnInterval * 1000).toInt()), (timer) {
      if (!mounted || !_state.isPlaying) {
        timer.cancel();
        return;
      }

      _spawnNote();
    });
  }

  void _spawnNote() {
    final lane = _random.nextInt(_numLanes);
    final instrument =
        JazzInstrument.values[_random.nextInt(JazzInstrument.values.length)];
    final note = JazzNote(
      lane: lane,
      spawnTime: _state.gameTime,
      targetTime: _state.gameTime + 2.0, // 2 seconds to hit
      instrument: instrument,
    );

    setState(() {
      _state = _state.copyWith(
        notes: [..._state.notes, note],
      );
    });
  }

  void _hitLane(int lane) {
    if (!_state.isPlaying) return;

    // Find the closest note in this lane
    JazzNote? closestNote;
    double closestDiff = double.infinity;

    for (final note in _state.notes) {
      if (note.lane == lane && !note.hit) {
        final diff = (note.targetTime - _state.gameTime).abs();
        if (diff < closestDiff && diff < 0.2) {
          closestNote = note;
          closestDiff = diff;
        }
      }
    }

    if (closestNote != null) {
      final accuracy = closestNote.getAccuracy(_state.gameTime);
      final points = accuracy.points * (_state.combo + 1);

      setState(() {
        _lastHit = accuracy;
        _state = _state.copyWith(
          score: _state.score + points,
          combo: accuracy == JazzHitAccuracy.miss ? 0 : _state.combo + 1,
          maxCombo: max(_state.maxCombo, _state.combo + 1),
          notes: _state.notes.map((n) {
            if (n == closestNote) {
              return n.copyWith(hit: true);
            }
            return n;
          }).toList(),
        );
      });

      // Clear last hit after animation
      Future.delayed(const Duration(milliseconds: 500), () {
        if (mounted) {
          setState(() {
            _lastHit = null;
          });
        }
      });
    }
  }

  void _endGame() async {
    setState(() {
      _state = _state.copyWith(isPlaying: false);
    });
    _gameTimer?.cancel();
    _noteSpawner?.cancel();

    // Track achievement
    if (_state.score > 1000) {
      await incrementAchievement(ref, 'explorer');
    }

    if (!mounted) return;

    // Show results
    showDialog(
      context: context,
      barrierDismissible: false,
      builder: (context) => AlertDialog(
        title: const Text('Game Over!'),
        content: Column(
          mainAxisSize: MainAxisSize.min,
          children: [
            Text('Score: ${_state.score}',
                style: const TextStyle(fontSize: 24)),
            const SizedBox(height: 8),
            Text('Max Combo: ${_state.maxCombo}x'),
            const SizedBox(height: 8),
            Text(_getPerformanceMessage()),
          ],
        ),
        actions: [
          TextButton(
            onPressed: () {
              Navigator.pop(context);
              _startGame();
            },
            child: const Text('Play Again'),
          ),
          TextButton(
            onPressed: () {
              Navigator.pop(context);
              Navigator.pop(context);
            },
            child: const Text('Exit'),
          ),
        ],
      ),
    );
  }

  String _getPerformanceMessage() {
    if (_state.score >= 2000) return 'Legendary! You are a jazz master!';
    if (_state.score >= 1500) return 'Excellent! Smooth as bebop!';
    if (_state.score >= 1000) return 'Great! You have got rhythm!';
    if (_state.score >= 500) return 'Not bad! Keep practicing!';
    return 'Keep trying! Everyone starts somewhere!';
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: Text('Jazz Rhythm - ${widget.difficulty.displayName}'),
        actions: [
          if (_state.isPlaying)
            IconButton(
              icon: const Icon(Icons.stop),
              onPressed: _endGame,
            ),
        ],
      ),
      body: SafeArea(
        child: _state.isPlaying ? _buildGame() : _buildStartScreen(),
      ),
    );
  }

  Widget _buildStartScreen() {
    return Center(
      child: Padding(
        padding: const EdgeInsets.all(24),
        child: Column(
          mainAxisAlignment: MainAxisAlignment.center,
          children: [
            const Text(
              '🎷',
              style: TextStyle(fontSize: 80),
            ),
            const SizedBox(height: 24),
            Text(
              'Jazz Rhythm Game',
              style: Theme.of(context).textTheme.headlineMedium?.copyWith(
                    color: UrbanColors.neonCyan,
                    fontWeight: FontWeight.bold,
                  ),
            ),
            const SizedBox(height: 16),
            Text(
              widget.difficulty.description,
              style: Theme.of(context).textTheme.bodyLarge,
              textAlign: TextAlign.center,
            ),
            const SizedBox(height: 32),
            Container(
              padding: const EdgeInsets.all(16),
              decoration: BoxDecoration(
                color: UrbanColors.concreteGray,
                border: Border.all(color: UrbanColors.comicBlack, width: 2),
                borderRadius: BorderRadius.circular(8),
              ),
              child: Column(
                crossAxisAlignment: CrossAxisAlignment.start,
                children: [
                  Text(
                    'How to Play:',
                    style: Theme.of(context).textTheme.titleMedium?.copyWith(
                          fontWeight: FontWeight.bold,
                        ),
                  ),
                  const SizedBox(height: 8),
                  const Text('• Watch notes fall down lanes'),
                  const Text('• Tap lanes when notes hit the target line'),
                  const Text('• Build combos for bonus points'),
                  const Text('• Perfect timing = more points!'),
                ],
              ),
            ),
            const SizedBox(height: 32),
            ElevatedButton(
              onPressed: _startGame,
              style: ElevatedButton.styleFrom(
                padding:
                    const EdgeInsets.symmetric(horizontal: 48, vertical: 16),
                backgroundColor: UrbanColors.neonCyan,
                foregroundColor: UrbanColors.comicBlack,
              ),
              child: const Text(
                'START GAME',
                style: TextStyle(fontSize: 20, fontWeight: FontWeight.bold),
              ),
            ),
          ],
        ),
      ),
    );
  }

  Widget _buildGame() {
    return Column(
      children: [
        // Score and combo
        Container(
          padding: const EdgeInsets.all(16),
          color: UrbanColors.concreteGray,
          child: Row(
            mainAxisAlignment: MainAxisAlignment.spaceBetween,
            children: [
              Column(
                crossAxisAlignment: CrossAxisAlignment.start,
                children: [
                  Text(
                    'Score',
                    style: Theme.of(context).textTheme.labelSmall,
                  ),
                  Text(
                    '${_state.score}',
                    style: Theme.of(context).textTheme.titleLarge?.copyWith(
                          fontWeight: FontWeight.bold,
                          color: UrbanColors.neonYellow,
                        ),
                  ),
                ],
              ),
              if (_state.combo > 0)
                Container(
                  padding:
                      const EdgeInsets.symmetric(horizontal: 12, vertical: 6),
                  decoration: BoxDecoration(
                    color: _state.combo > 10
                        ? UrbanColors.neonCyan
                        : UrbanColors.warningOrange,
                    border: Border.all(color: UrbanColors.comicBlack, width: 2),
                    borderRadius: BorderRadius.circular(16),
                  ),
                  child: Text(
                    '${_state.combo}x COMBO',
                    style: const TextStyle(
                      fontWeight: FontWeight.bold,
                      color: UrbanColors.comicBlack,
                    ),
                  ),
                ),
              if (_lastHit != null)
                TweenAnimationBuilder(
                  duration: const Duration(milliseconds: 500),
                  tween: Tween<double>(begin: 1.5, end: 0.0),
                  builder: (context, double value, child) {
                    return Opacity(
                      opacity: 1.0 - value / 1.5,
                      child: Transform.translate(
                        offset: Offset(0, -value * 30),
                        child: Text(
                          _lastHit!.displayName,
                          style: TextStyle(
                            fontSize: 20 + value * 10,
                            fontWeight: FontWeight.bold,
                            color: _lastHit == JazzHitAccuracy.perfect
                                ? UrbanColors.neonCyan
                                : UrbanColors.neonYellow,
                          ),
                        ),
                      ),
                    );
                  },
                ),
            ],
          ),
        ),

        // Game area
        Expanded(
          child: Stack(
            children: [
              // Lanes
              Row(
                children: List.generate(_numLanes, (lane) {
                  return Expanded(
                    child: GestureDetector(
                      onTap: () => _hitLane(lane),
                      child: Container(
                        decoration: BoxDecoration(
                          border: Border.all(color: UrbanColors.fog),
                          color: UrbanColors.asphalt.withOpacity(0.3),
                        ),
                        child: CustomPaint(
                          painter: _NoteLanePainter(
                            notes: _state.notes
                                .where((n) => n.lane == lane && !n.hit)
                                .toList(),
                            gameTime: _state.gameTime,
                          ),
                        ),
                      ),
                    ),
                  );
                }),
              ),

              // Target line
              Positioned(
                bottom: 100,
                left: 0,
                right: 0,
                child: Container(
                  height: 4,
                  decoration: BoxDecoration(
                    color: UrbanColors.neonCyan,
                    boxShadow: [
                      BoxShadow(
                        color: UrbanColors.neonCyan.withOpacity(0.5),
                        blurRadius: 10,
                        spreadRadius: 2,
                      ),
                    ],
                  ),
                ),
              ),
            ],
          ),
        ),
      ],
    );
  }
}

/// Custom painter for notes falling down lanes
class _NoteLanePainter extends CustomPainter {
  final List<JazzNote> notes;
  final double gameTime;

  _NoteLanePainter({required this.notes, required this.gameTime});

  @override
  void paint(Canvas canvas, Size size) {
    for (final note in notes) {
      final progress =
          (gameTime - note.spawnTime) / (note.targetTime - note.spawnTime);
      final y = progress * size.height;

      if (y >= 0 && y <= size.height) {
        final paint = Paint()
          ..color = _getInstrumentColor(note.instrument)
          ..style = PaintingStyle.fill;

        final rect = RRect.fromRectAndRadius(
          Rect.fromCenter(
            center: Offset(size.width / 2, y),
            width: size.width * 0.8,
            height: 60,
          ),
          const Radius.circular(8),
        );

        canvas.drawRRect(rect, paint);

        // Border
        final borderPaint = Paint()
          ..color = UrbanColors.comicBlack
          ..style = PaintingStyle.stroke
          ..strokeWidth = 2;
        canvas.drawRRect(rect, borderPaint);
      }
    }
  }

  Color _getInstrumentColor(JazzInstrument instrument) {
    switch (instrument) {
      case JazzInstrument.snare:
        return UrbanColors.warningOrange;
      case JazzInstrument.cymbal:
        return UrbanColors.neonYellow;
      case JazzInstrument.bass:
        return UrbanColors.neonCyan;
      case JazzInstrument.piano:
        return UrbanColors.neonMagenta;
    }
  }

  @override
  bool shouldRepaint(_NoteLanePainter oldDelegate) => true;
}
