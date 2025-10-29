import 'package:flutter/material.dart';
import 'package:google_fonts/google_fonts.dart';
import 'urban_colors.dart';

/// Urban underground comic text styles
/// High contrast, graffiti-inspired typography
class UrbanTextStyles {
  // Graffiti-style display text (for big titles)
  static TextStyle get graffitiDisplay => GoogleFonts.bangers(
    fontSize: 48,
    fontWeight: FontWeight.bold,
    letterSpacing: 2.0,
    height: 1.1,
    color: UrbanColors.comicWhite,
    shadows: const [
      Shadow(
        color: UrbanColors.neonCyan,
        offset: Offset(3, 3),
        blurRadius: 8,
      ),
      Shadow(
        color: UrbanColors.shadow,
        offset: Offset(6, 6),
        blurRadius: 4,
      ),
    ],
  );

  // Street-style headlines
  static TextStyle get streetHeadline => GoogleFonts.bangers(
    fontSize: 32,
    fontWeight: FontWeight.bold,
    letterSpacing: 1.5,
    height: 1.2,
    color: UrbanColors.rustOrange,
    shadows: const [
      Shadow(
        color: UrbanColors.shadow,
        offset: Offset(2, 2),
        blurRadius: 4,
      ),
    ],
  );

  // Comic panel titles
  static TextStyle get comicTitle => GoogleFonts.permanentMarker(
    fontSize: 24,
    fontWeight: FontWeight.w600,
    letterSpacing: 1.0,
    height: 1.3,
    color: UrbanColors.comicWhite,
    shadows: const [
      Shadow(
        color: UrbanColors.comicBlack,
        offset: Offset(2, 2),
        blurRadius: 1,
      ),
    ],
  );

  // Body text with high contrast
  static TextStyle get urbanBody => GoogleFonts.robotoCondensed(
    fontSize: 16,
    fontWeight: FontWeight.w500,
    letterSpacing: 0.5,
    height: 1.5,
    color: UrbanColors.moonlight,
  );

  // Small caps for labels
  static TextStyle get urbanLabel => GoogleFonts.robotoCondensed(
    fontSize: 12,
    fontWeight: FontWeight.w700,
    letterSpacing: 1.5,
    height: 1.4,
    color: UrbanColors.fog,
  );

  // Speech bubble text
  static TextStyle get speechBubble => GoogleFonts.permanentMarker(
    fontSize: 14,
    fontWeight: FontWeight.w500,
    letterSpacing: 0.5,
    height: 1.4,
    color: UrbanColors.comicBlack,
  );

  // Neon sign style (for important CTAs)
  static TextStyle get neonSign => GoogleFonts.bangers(
    fontSize: 20,
    fontWeight: FontWeight.bold,
    letterSpacing: 2.0,
    height: 1.2,
    color: UrbanColors.neonCyan,
    shadows: const [
      Shadow(
        color: UrbanColors.neonCyan,
        offset: Offset(0, 0),
        blurRadius: 20,
      ),
      Shadow(
        color: UrbanColors.neonCyan,
        offset: Offset(0, 0),
        blurRadius: 40,
      ),
    ],
  );

  // Stencil-style text (like spray paint)
  static TextStyle get stencilText => GoogleFonts.bangers(
    fontSize: 18,
    fontWeight: FontWeight.w600,
    letterSpacing: 1.2,
    height: 1.3,
    color: UrbanColors.rustOrange,
  );

  // Helper method to create text with outline (comic book style)
  static TextStyle withComicOutline(TextStyle base, {Color? outlineColor}) {
    return base.copyWith(
      foreground: Paint()
        ..style = PaintingStyle.stroke
        ..strokeWidth = 2
        ..color = outlineColor ?? UrbanColors.comicBlack,
      shadows: [
        Shadow(
          color: outlineColor ?? UrbanColors.comicBlack,
          offset: const Offset(1, 1),
          blurRadius: 2,
        ),
      ],
    );
  }

  // High contrast button text
  static TextStyle get buttonText => GoogleFonts.bangers(
    fontSize: 16,
    fontWeight: FontWeight.bold,
    letterSpacing: 1.5,
    height: 1.0,
    color: UrbanColors.comicWhite,
  );
}
