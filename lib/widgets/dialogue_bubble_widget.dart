import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:google_fonts/google_fonts.dart';
import '../providers/dialogue_provider.dart';
import '../theme/urban_colors.dart';
import '../theme/comic_decorations.dart';

/// Displays dog dialogue in a comic-style speech bubble
class DialogueBubbleWidget extends ConsumerStatefulWidget {
  final Duration displayDuration;
  final VoidCallback? onDismiss;

  const DialogueBubbleWidget({
    super.key,
    this.displayDuration = const Duration(seconds: 5),
    this.onDismiss,
  });

  @override
  ConsumerState<DialogueBubbleWidget> createState() =>
      _DialogueBubbleWidgetState();
}

class _DialogueBubbleWidgetState extends ConsumerState<DialogueBubbleWidget>
    with SingleTickerProviderStateMixin {
  late AnimationController _animController;
  late Animation<double> _scaleAnimation;
  late Animation<double> _opacityAnimation;

  @override
  void initState() {
    super.initState();
    _setupAnimations();
  }

  void _setupAnimations() {
    _animController = AnimationController(
      vsync: this,
      duration: const Duration(milliseconds: 300),
    );

    _scaleAnimation = Tween<double>(begin: 0.8, end: 1.0).animate(
      CurvedAnimation(parent: _animController, curve: Curves.elasticOut),
    );

    _opacityAnimation = Tween<double>(begin: 0.0, end: 1.0).animate(
      CurvedAnimation(parent: _animController, curve: Curves.easeIn),
    );
  }

  @override
  void dispose() {
    _animController.dispose();
    super.dispose();
  }

  void _showBubble() {
    _animController.forward();

    // Auto-dismiss after duration
    Future.delayed(widget.displayDuration, () {
      if (mounted) {
        _animController.reverse().then((_) {
          ref.read(dialogueProvider.notifier).clearDialogue();
          widget.onDismiss?.call();
        });
      }
    });
  }

  @override
  Widget build(BuildContext context) {
    final dialogueState = ref.watch(dialogueProvider);
    final dialogue = dialogueState.currentDialogue;

    if (dialogue == null || dialogue.isEmpty) {
      return const SizedBox.shrink();
    }

    // Trigger animation when new dialogue appears
    if (_animController.status == AnimationStatus.dismissed) {
      WidgetsBinding.instance.addPostFrameCallback((_) {
        _showBubble();
      });
    }

    return AnimatedBuilder(
      animation: _animController,
      builder: (context, child) {
        return Opacity(
          opacity: _opacityAnimation.value,
          child: Transform.scale(
            scale: _scaleAnimation.value,
            child: child,
          ),
        );
      },
      child: _buildBubble(dialogue),
    );
  }

  Widget _buildBubble(String dialogue) {
    return Container(
      constraints: const BoxConstraints(maxWidth: 300),
      padding: const EdgeInsets.all(16),
      decoration: ComicDecorations.speechBubble(
        backgroundColor: UrbanColors.comicWhite,
        borderColor: UrbanColors.comicBlack,
        borderWidth: 3,
      ),
      child: Column(
        mainAxisSize: MainAxisSize.min,
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          Text(
            dialogue,
            style: GoogleFonts.permanentMarker(
              fontSize: 14,
              fontWeight: FontWeight.w500,
              letterSpacing: 0.5,
              height: 1.4,
              color: UrbanColors.comicBlack,
            ),
          ),
          const SizedBox(height: 4),
          // Tail pointer (points to dog)
          Align(
            alignment: Alignment.bottomLeft,
            child: CustomPaint(
              size: const Size(20, 10),
              painter: _SpeechBubbleTailPainter(),
            ),
          ),
        ],
      ),
    );
  }
}

/// Paints the speech bubble tail
class _SpeechBubbleTailPainter extends CustomPainter {
  @override
  void paint(Canvas canvas, Size size) {
    final paint = Paint()
      ..color = UrbanColors.comicWhite
      ..style = PaintingStyle.fill;

    final borderPaint = Paint()
      ..color = UrbanColors.comicBlack
      ..style = PaintingStyle.stroke
      ..strokeWidth = 3
      ..strokeCap = StrokeCap.round
      ..strokeJoin = StrokeJoin.round;

    final path = Path()
      ..moveTo(0, 0)
      ..lineTo(size.width / 2, size.height)
      ..lineTo(size.width, 0)
      ..close();

    canvas.drawPath(path, paint);
    canvas.drawPath(path, borderPaint);
  }

  @override
  bool shouldRepaint(covariant CustomPainter oldDelegate) => false;
}

/// Positioned dialogue bubble widget for use in screens
class PositionedDialogueBubble extends StatelessWidget {
  final Alignment alignment;
  final EdgeInsets margin;

  const PositionedDialogueBubble({
    super.key,
    this.alignment = Alignment.topCenter,
    this.margin = const EdgeInsets.all(16),
  });

  @override
  Widget build(BuildContext context) {
    return Align(
      alignment: alignment,
      child: Padding(
        padding: margin,
        child: const DialogueBubbleWidget(),
      ),
    );
  }
}
