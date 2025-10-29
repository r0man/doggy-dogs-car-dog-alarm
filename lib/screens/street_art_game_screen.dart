import 'dart:async';
import 'dart:ui' as ui;
import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import '../models/street_art_game.dart';
import '../theme/urban_colors.dart';
import '../providers/achievement_provider.dart';

/// Street art tagging mini-game screen
class StreetArtGameScreen extends ConsumerStatefulWidget {
  const StreetArtGameScreen({super.key});

  @override
  ConsumerState<StreetArtGameScreen> createState() =>
      _StreetArtGameScreenState();
}

class _StreetArtGameScreenState extends ConsumerState<StreetArtGameScreen> {
  late StreetArtGameState _state;
  Color _selectedColor = SprayPaintColors.neonPink;
  Timer? _gameTimer;
  final List<ArtStroke> _currentStrokes = [];
  List<Offset>? _currentStrokePoints;

  @override
  void initState() {
    super.initState();
    _state = const StreetArtGameState();
  }

  @override
  void dispose() {
    _gameTimer?.cancel();
    super.dispose();
  }

  void _startGame() {
    setState(() {
      _state = StreetArtGameState(
        isPlaying: true,
        challenges: ArtChallenges.all,
        timeRemaining: ArtChallenges.all.first.timeLimit,
      );
      _currentStrokes.clear();
    });

    // Game timer
    _gameTimer = Timer.periodic(const Duration(milliseconds: 100), (timer) {
      if (!mounted || !_state.isPlaying) {
        timer.cancel();
        return;
      }

      setState(() {
        final newTime = _state.timeRemaining - 0.1;
        if (newTime <= 0) {
          _completeChallenge();
        } else {
          _state = _state.copyWith(timeRemaining: newTime);
        }
      });
    });
  }

  void _completeChallenge() {
    final challenge = _state.currentChallenge;
    if (challenge == null) return;

    // Calculate score based on time and strokes
    final timeFactor = _state.timeRemaining / challenge.timeLimit;
    final strokeFactor = _currentStrokes.isEmpty ? 0.0 : 1.0;
    final earnedPoints = (challenge.points * timeFactor * strokeFactor).toInt();

    setState(() {
      _state = _state.copyWith(
        score: _state.score + earnedPoints,
        currentChallengeIndex: _state.currentChallengeIndex + 1,
        timeRemaining: _state.currentChallenge?.timeLimit ?? 0,
        strokes: [],
      );
      _currentStrokes.clear();
    });

    if (_state.isComplete) {
      _endGame();
    }
  }

  void _endGame() async {
    _gameTimer?.cancel();
    setState(() {
      _state = _state.copyWith(isPlaying: false);
    });

    // Track achievement
    if (_state.score > 500) {
      await incrementAchievement(ref, 'explorer');
    }

    if (!mounted) return;

    // Show results
    showDialog(
      context: context,
      barrierDismissible: false,
      builder: (context) => AlertDialog(
        title: const Text('Gallery Complete!'),
        content: Column(
          mainAxisSize: MainAxisSize.min,
          children: [
            const Text('🎨', style: TextStyle(fontSize: 48)),
            const SizedBox(height: 16),
            Text(
              'Total Score: ${_state.score}',
              style: const TextStyle(fontSize: 24, fontWeight: FontWeight.bold),
            ),
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
    if (_state.score >= 800) return 'Master street artist! Legendary work!';
    if (_state.score >= 600) return 'Impressive! Gallery-worthy pieces!';
    if (_state.score >= 400) return 'Nice! You have got style!';
    if (_state.score >= 200) return 'Good start! Keep practicing!';
    return 'Art is about expression! Try again!';
  }

  void _skipChallenge() {
    _completeChallenge();
  }

  void _clearCanvas() {
    setState(() {
      _currentStrokes.clear();
    });
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: const Text('Tag the Tagged'),
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
              '🎨',
              style: TextStyle(fontSize: 80),
            ),
            const SizedBox(height: 24),
            Text(
              'Tag the Tagged',
              style: Theme.of(context).textTheme.headlineMedium?.copyWith(
                    color: UrbanColors.neonCyan,
                    fontWeight: FontWeight.bold,
                  ),
            ),
            const SizedBox(height: 16),
            Text(
              'Express yourself through street art',
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
                  const Text('• Complete art challenges'),
                  const Text('• Draw with your finger'),
                  const Text('• Choose from spray paint colors'),
                  const Text('• Beat the timer for bonus points'),
                  const Text('• Express your urban creativity!'),
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
                'START TAGGING',
                style: TextStyle(fontSize: 20, fontWeight: FontWeight.bold),
              ),
            ),
          ],
        ),
      ),
    );
  }

  Widget _buildGame() {
    final challenge = _state.currentChallenge;
    if (challenge == null) return const SizedBox();

    return Column(
      children: [
        // Challenge info
        Container(
          padding: const EdgeInsets.all(16),
          color: UrbanColors.concreteGray,
          child: Column(
            crossAxisAlignment: CrossAxisAlignment.stretch,
            children: [
              Row(
                mainAxisAlignment: MainAxisAlignment.spaceBetween,
                children: [
                  Expanded(
                    child: Column(
                      crossAxisAlignment: CrossAxisAlignment.start,
                      children: [
                        Row(
                          children: [
                            Text(
                              challenge.icon,
                              style: const TextStyle(fontSize: 32),
                            ),
                            const SizedBox(width: 12),
                            Expanded(
                              child: Text(
                                challenge.prompt,
                                style: Theme.of(context)
                                    .textTheme
                                    .titleMedium
                                    ?.copyWith(
                                      fontWeight: FontWeight.bold,
                                    ),
                              ),
                            ),
                          ],
                        ),
                        const SizedBox(height: 4),
                        Text(
                          challenge.style.displayName,
                          style:
                              Theme.of(context).textTheme.bodySmall?.copyWith(
                                    color: UrbanColors.fog,
                                  ),
                        ),
                      ],
                    ),
                  ),
                  Column(
                    children: [
                      Text(
                        '${_state.timeRemaining.toStringAsFixed(1)}s',
                        style:
                            Theme.of(context).textTheme.headlineSmall?.copyWith(
                                  color: _state.timeRemaining < 10
                                      ? UrbanColors.dangerRed
                                      : UrbanColors.neonYellow,
                                  fontWeight: FontWeight.bold,
                                ),
                      ),
                      Text(
                        'Score: ${_state.score}',
                        style: Theme.of(context).textTheme.bodySmall,
                      ),
                    ],
                  ),
                ],
              ),
              const SizedBox(height: 12),
              LinearProgressIndicator(
                value: _state.timeRemaining / challenge.timeLimit,
                backgroundColor: UrbanColors.asphalt,
                valueColor: AlwaysStoppedAnimation<Color>(
                  _state.timeRemaining < 10
                      ? UrbanColors.dangerRed
                      : UrbanColors.neonCyan,
                ),
              ),
            ],
          ),
        ),

        // Canvas
        Expanded(
          child: Container(
            color: UrbanColors.asphalt,
            child: GestureDetector(
              onPanStart: (details) {
                setState(() {
                  _currentStrokePoints = [details.localPosition];
                });
              },
              onPanUpdate: (details) {
                setState(() {
                  _currentStrokePoints?.add(details.localPosition);
                });
              },
              onPanEnd: (details) {
                if (_currentStrokePoints != null &&
                    _currentStrokePoints!.isNotEmpty) {
                  setState(() {
                    _currentStrokes.add(ArtStroke(
                      points: List.from(_currentStrokePoints!),
                      color: _selectedColor,
                      width: 8.0,
                    ));
                    _currentStrokePoints = null;
                  });
                }
              },
              child: CustomPaint(
                painter: _ArtCanvasPainter(
                  strokes: _currentStrokes,
                  currentStroke: _currentStrokePoints != null
                      ? ArtStroke(
                          points: _currentStrokePoints!,
                          color: _selectedColor,
                          width: 8.0,
                        )
                      : null,
                ),
                child: Container(),
              ),
            ),
          ),
        ),

        // Color palette
        Container(
          padding: const EdgeInsets.all(16),
          color: UrbanColors.concreteGray,
          child: Column(
            children: [
              Row(
                mainAxisAlignment: MainAxisAlignment.spaceEvenly,
                children: SprayPaintColors.all.map((color) {
                  final isSelected = color == _selectedColor;
                  return GestureDetector(
                    onTap: () {
                      setState(() {
                        _selectedColor = color;
                      });
                    },
                    child: Container(
                      width: 40,
                      height: 40,
                      decoration: BoxDecoration(
                        color: color,
                        shape: BoxShape.circle,
                        border: Border.all(
                          color: isSelected
                              ? UrbanColors.comicWhite
                              : UrbanColors.comicBlack,
                          width: isSelected ? 4 : 2,
                        ),
                        boxShadow: isSelected
                            ? [
                                BoxShadow(
                                  color: color.withOpacity(0.5),
                                  blurRadius: 8,
                                  spreadRadius: 2,
                                ),
                              ]
                            : null,
                      ),
                    ),
                  );
                }).toList(),
              ),
              const SizedBox(height: 12),
              Row(
                children: [
                  Expanded(
                    child: OutlinedButton.icon(
                      onPressed: _clearCanvas,
                      icon: const Icon(Icons.clear),
                      label: const Text('Clear'),
                    ),
                  ),
                  const SizedBox(width: 8),
                  Expanded(
                    child: ElevatedButton.icon(
                      onPressed: _completeChallenge,
                      icon: const Icon(Icons.check),
                      label: const Text('Done'),
                      style: ElevatedButton.styleFrom(
                        backgroundColor: UrbanColors.successGreen,
                      ),
                    ),
                  ),
                  const SizedBox(width: 8),
                  Expanded(
                    child: OutlinedButton.icon(
                      onPressed: _skipChallenge,
                      icon: const Icon(Icons.skip_next),
                      label: const Text('Skip'),
                    ),
                  ),
                ],
              ),
            ],
          ),
        ),
      ],
    );
  }
}

/// Custom painter for the art canvas
class _ArtCanvasPainter extends CustomPainter {
  final List<ArtStroke> strokes;
  final ArtStroke? currentStroke;

  _ArtCanvasPainter({
    required this.strokes,
    this.currentStroke,
  });

  @override
  void paint(Canvas canvas, Size size) {
    // Draw completed strokes
    for (final stroke in strokes) {
      _drawStroke(canvas, stroke);
    }

    // Draw current stroke
    if (currentStroke != null) {
      _drawStroke(canvas, currentStroke!);
    }
  }

  void _drawStroke(Canvas canvas, ArtStroke stroke) {
    if (stroke.points.length < 2) return;

    final paint = Paint()
      ..color = stroke.color
      ..strokeWidth = stroke.width
      ..strokeCap = StrokeCap.round
      ..strokeJoin = StrokeJoin.round
      ..style = PaintingStyle.stroke;

    final path = Path();
    path.moveTo(stroke.points.first.dx, stroke.points.first.dy);

    for (var i = 1; i < stroke.points.length; i++) {
      path.lineTo(stroke.points[i].dx, stroke.points[i].dy);
    }

    canvas.drawPath(path, paint);
  }

  @override
  bool shouldRepaint(_ArtCanvasPainter oldDelegate) => true;
}
