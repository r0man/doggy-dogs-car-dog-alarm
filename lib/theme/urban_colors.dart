import 'package:flutter/material.dart';

/// Urban underground comic color palette
/// Inspired by concrete jungles, street art, and noir aesthetics
class UrbanColors {
  // Primary urban palette
  static const concreteGray = Color(0xFF2B2D2F); // Dark concrete
  static const concreteLight = Color(0xFF4A4E52); // Lighter concrete
  static const concreteDark = Color(0xFF1A1C1E); // Almost black concrete

  static const rustOrange = Color(0xFFE85D27); // Rust/oxidized metal
  static const rustDark = Color(0xFFB84518); // Darker rust
  static const rustLight = Color(0xFFFF7543); // Lighter rust

  // Neon accents for high contrast
  static const neonCyan = Color(0xFF00F5FF); // Electric cyan
  static const neonPink = Color(0xFFFF10F0); // Hot pink
  static const neonMagenta = Color(0xFF9D00FF); // Electric magenta
  static const neonGreen = Color(0xFF39FF14); // Toxic green
  static const neonYellow = Color(0xFFFFFF00); // Warning yellow

  // Urban neutrals
  static const asphalt = Color(0xFF36393F); // Street asphalt
  static const graffiti = Color(0xFF52575D); // Spray paint gray
  static const shadow = Color(0xFF0D0F12); // Deep noir shadow
  static const fog = Color(0xFF72767D); // Urban fog/smog

  // Status colors with urban twist
  static const dangerRed = Color(0xFFED4245); // Aggressive red
  static const warningOrange = Color(0xFFFFA500); // Caution tape orange
  static const successGreen = Color(0xFF57F287); // Radioactive green

  // Night-time emphasis
  static const nightSky = Color(0xFF0A0B0D); // Dark night
  static const streetLight = Color(0xFFFFA947); // Sodium vapor light
  static const moonlight = Color(0xFFE0E6ED); // Cold moonlight

  // Halftone/comic book colors
  static const comicBlack = Color(0xFF000000);
  static const comicWhite = Color(0xFFFFFFFF);
  static const halftoneGray = Color(0xFF808080);

  // Gradient combinations for dynamic effects
  static const urbanGradient = LinearGradient(
    colors: [concreteDark, concreteGray],
    begin: Alignment.topLeft,
    end: Alignment.bottomRight,
  );

  static const neonGradient = LinearGradient(
    colors: [neonCyan, neonPink],
    begin: Alignment.topLeft,
    end: Alignment.bottomRight,
  );

  static const rustGradient = LinearGradient(
    colors: [rustDark, rustOrange, rustLight],
    begin: Alignment.topCenter,
    end: Alignment.bottomCenter,
  );
}
