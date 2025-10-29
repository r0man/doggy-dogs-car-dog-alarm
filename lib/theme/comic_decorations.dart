import 'package:flutter/material.dart';
import 'package:google_fonts/google_fonts.dart';
import 'urban_colors.dart';

/// Comic book and street art decorations
/// Includes halftone patterns, speech bubbles, noir shadows, etc.
class ComicDecorations {
  // Standard drop shadow
  static const BoxShadow dropShadow = BoxShadow(
    color: UrbanColors.shadow,
    offset: Offset(4, 4),
    blurRadius: 0,
  );

  // Speech bubble decoration
  static BoxDecoration speechBubble({
    Color backgroundColor = UrbanColors.comicWhite,
    Color borderColor = UrbanColors.comicBlack,
    double borderWidth = 3,
  }) {
    return BoxDecoration(
      color: backgroundColor,
      border: Border.all(
        color: borderColor,
        width: borderWidth,
      ),
      borderRadius: BorderRadius.circular(12),
      boxShadow: const [
        BoxShadow(
          color: UrbanColors.shadow,
          offset: Offset(4, 4),
          blurRadius: 0, // No blur for comic style
        ),
      ],
    );
  }

  // Comic panel border (thick black outline)
  static BoxDecoration comicPanel({
    Color backgroundColor = UrbanColors.concreteGray,
    Color borderColor = UrbanColors.comicBlack,
    double borderWidth = 4,
  }) {
    return BoxDecoration(
      color: backgroundColor,
      border: Border.all(
        color: borderColor,
        width: borderWidth,
      ),
      borderRadius: BorderRadius.circular(4),
      boxShadow: const [
        BoxShadow(
          color: UrbanColors.shadow,
          offset: Offset(8, 8),
          blurRadius: 0, // Hard shadow for comic effect
        ),
      ],
    );
  }

  // Noir shadow effect (strong directional shadow)
  static List<BoxShadow> noirShadow({
    Color shadowColor = UrbanColors.shadow,
    double offsetX = 6,
    double offsetY = 6,
    double blurRadius = 4,
  }) {
    return [
      BoxShadow(
        color: shadowColor,
        offset: Offset(offsetX, offsetY),
        blurRadius: blurRadius,
        spreadRadius: 2,
      ),
    ];
  }

  // Neon glow effect
  static List<BoxShadow> neonGlow({
    Color glowColor = UrbanColors.neonCyan,
    double intensity = 1.0,
  }) {
    return [
      BoxShadow(
        color: glowColor.withOpacity(0.8 * intensity),
        offset: const Offset(0, 0),
        blurRadius: 20,
        spreadRadius: 2,
      ),
      BoxShadow(
        color: glowColor.withOpacity(0.6 * intensity),
        offset: const Offset(0, 0),
        blurRadius: 40,
        spreadRadius: 4,
      ),
      BoxShadow(
        color: glowColor.withOpacity(0.4 * intensity),
        offset: const Offset(0, 0),
        blurRadius: 60,
        spreadRadius: 6,
      ),
    ];
  }

  // Graffiti-style border (irregular, spray-paint effect simulation)
  static BoxDecoration graffitiBox({
    Color backgroundColor = UrbanColors.asphalt,
    Color borderColor = UrbanColors.rustOrange,
  }) {
    return BoxDecoration(
      color: backgroundColor,
      border: Border.all(
        color: borderColor,
        width: 3,
      ),
      borderRadius: BorderRadius.circular(2),
      boxShadow: [
        BoxShadow(
          color: borderColor.withOpacity(0.3),
          offset: const Offset(0, 0),
          blurRadius: 8,
          spreadRadius: 2,
        ),
      ],
    );
  }

  // Urban button decoration (for CTAs)
  static BoxDecoration urbanButton({
    Color backgroundColor = UrbanColors.rustOrange,
    Color borderColor = UrbanColors.comicBlack,
    bool isPressed = false,
  }) {
    return BoxDecoration(
      color: backgroundColor,
      border: Border.all(
        color: borderColor,
        width: 3,
      ),
      borderRadius: BorderRadius.circular(4),
      boxShadow: isPressed
          ? const [
              BoxShadow(
                color: UrbanColors.shadow,
                offset: Offset(2, 2),
                blurRadius: 0,
              ),
            ]
          : const [
              BoxShadow(
                color: UrbanColors.shadow,
                offset: Offset(6, 6),
                blurRadius: 0,
              ),
            ],
    );
  }

  // Street sign style (caution tape aesthetic)
  static BoxDecoration cautionTape() {
    return const BoxDecoration(
      gradient: LinearGradient(
        colors: [
          UrbanColors.warningOrange,
          UrbanColors.comicBlack,
          UrbanColors.warningOrange,
          UrbanColors.comicBlack,
        ],
        stops: [0.0, 0.25, 0.5, 0.75],
        begin: Alignment.topLeft,
        end: Alignment.bottomRight,
      ),
      border: Border(
        top: BorderSide(color: UrbanColors.comicBlack, width: 2),
        bottom: BorderSide(color: UrbanColors.comicBlack, width: 2),
      ),
    );
  }
}

/// Custom painters for halftone and comic effects
class HalftonePainter extends CustomPainter {
  final Color dotColor;
  final double dotSize;
  final double spacing;

  HalftonePainter({
    this.dotColor = UrbanColors.halftoneGray,
    this.dotSize = 4,
    this.spacing = 8,
  });

  @override
  void paint(Canvas canvas, Size size) {
    final paint = Paint()
      ..color = dotColor
      ..style = PaintingStyle.fill;

    for (double x = 0; x < size.width; x += spacing) {
      for (double y = 0; y < size.height; y += spacing) {
        canvas.drawCircle(
          Offset(x, y),
          dotSize / 2,
          paint,
        );
      }
    }
  }

  @override
  bool shouldRepaint(covariant CustomPainter oldDelegate) => false;
}

/// Widget for halftone background overlay
class HalftoneOverlay extends StatelessWidget {
  final Widget child;
  final Color dotColor;
  final double dotSize;
  final double spacing;
  final double opacity;

  const HalftoneOverlay({
    super.key,
    required this.child,
    this.dotColor = UrbanColors.halftoneGray,
    this.dotSize = 4,
    this.spacing = 8,
    this.opacity = 0.1,
  });

  @override
  Widget build(BuildContext context) {
    return Stack(
      children: [
        child,
        Positioned.fill(
          child: Opacity(
            opacity: opacity,
            child: CustomPaint(
              painter: HalftonePainter(
                dotColor: dotColor,
                dotSize: dotSize,
                spacing: spacing,
              ),
            ),
          ),
        ),
      ],
    );
  }
}

/// Speech bubble widget with tail
class SpeechBubbleWidget extends StatelessWidget {
  final String text;
  final TextStyle? textStyle;
  final Color backgroundColor;
  final Color borderColor;
  final EdgeInsets padding;

  const SpeechBubbleWidget({
    super.key,
    required this.text,
    this.textStyle,
    this.backgroundColor = UrbanColors.comicWhite,
    this.borderColor = UrbanColors.comicBlack,
    this.padding = const EdgeInsets.all(16),
  });

  @override
  Widget build(BuildContext context) {
    return Container(
      padding: padding,
      decoration: ComicDecorations.speechBubble(
        backgroundColor: backgroundColor,
        borderColor: borderColor,
      ),
      child: Text(
        text,
        style: textStyle ?? Theme.of(context).textTheme.bodyMedium,
      ),
    );
  }
}

/// Comic panel widget
class ComicPanelWidget extends StatelessWidget {
  final Widget child;
  final Color backgroundColor;
  final Color borderColor;
  final EdgeInsets padding;

  const ComicPanelWidget({
    super.key,
    required this.child,
    this.backgroundColor = UrbanColors.concreteGray,
    this.borderColor = UrbanColors.comicBlack,
    this.padding = const EdgeInsets.all(16),
  });

  @override
  Widget build(BuildContext context) {
    return Container(
      padding: padding,
      decoration: ComicDecorations.comicPanel(
        backgroundColor: backgroundColor,
        borderColor: borderColor,
      ),
      child: child,
    );
  }
}

/// Neon sign text widget with glow effect
class NeonTextWidget extends StatelessWidget {
  final String text;
  final Color glowColor;
  final double fontSize;
  final double glowIntensity;

  const NeonTextWidget({
    super.key,
    required this.text,
    this.glowColor = UrbanColors.neonCyan,
    this.fontSize = 24,
    this.glowIntensity = 1.0,
  });

  @override
  Widget build(BuildContext context) {
    return Container(
      decoration: BoxDecoration(
        boxShadow: ComicDecorations.neonGlow(
          glowColor: glowColor,
          intensity: glowIntensity,
        ),
      ),
      child: Text(
        text,
        style: GoogleFonts.bangers(
          fontSize: fontSize,
          fontWeight: FontWeight.bold,
          letterSpacing: 2.0,
          color: glowColor,
        ),
      ),
    );
  }
}
