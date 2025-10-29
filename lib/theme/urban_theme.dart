import 'package:flutter/material.dart';
import 'package:flutter/services.dart';
import 'package:google_fonts/google_fonts.dart';
import 'urban_colors.dart';
import 'urban_text_styles.dart';

/// Urban underground comic theme
/// Dark, edgy, high-contrast with noir aesthetics
class UrbanTheme {
  static ThemeData get darkUrbanTheme {
    return ThemeData(
      // Use dark theme as base
      brightness: Brightness.dark,
      useMaterial3: true,

      // Urban color scheme
      colorScheme: const ColorScheme.dark(
        primary: UrbanColors.rustOrange,
        onPrimary: UrbanColors.comicWhite,
        primaryContainer: UrbanColors.rustDark,
        onPrimaryContainer: UrbanColors.comicWhite,

        secondary: UrbanColors.neonCyan,
        onSecondary: UrbanColors.comicBlack,
        secondaryContainer: UrbanColors.concreteLight,
        onSecondaryContainer: UrbanColors.neonCyan,

        tertiary: UrbanColors.neonPink,
        onTertiary: UrbanColors.comicBlack,

        error: UrbanColors.dangerRed,
        onError: UrbanColors.comicWhite,

        background: UrbanColors.nightSky,
        onBackground: UrbanColors.moonlight,

        surface: UrbanColors.concreteGray,
        onSurface: UrbanColors.comicWhite,

        surfaceVariant: UrbanColors.asphalt,
        onSurfaceVariant: UrbanColors.fog,

        outline: UrbanColors.graffiti,
        shadow: UrbanColors.shadow,
      ),

      // Scaffold background
      scaffoldBackgroundColor: UrbanColors.nightSky,

      // AppBar theme - urban street style
      appBarTheme: AppBarTheme(
        backgroundColor: UrbanColors.concreteDark,
        foregroundColor: UrbanColors.comicWhite,
        elevation: 8,
        shadowColor: UrbanColors.shadow,
        centerTitle: true,
        titleTextStyle: GoogleFonts.bangers(
          fontSize: 24,
          fontWeight: FontWeight.bold,
          letterSpacing: 1.5,
          color: UrbanColors.rustOrange,
          shadows: const [
            Shadow(
              color: UrbanColors.shadow,
              offset: Offset(2, 2),
              blurRadius: 4,
            ),
          ],
        ),
        iconTheme: const IconThemeData(
          color: UrbanColors.neonCyan,
          size: 24,
        ),
        systemOverlayStyle: SystemUiOverlayStyle.light,
      ),

      // Card theme - comic panel style
      cardTheme: CardTheme(
        color: UrbanColors.concreteGray,
        shadowColor: UrbanColors.shadow,
        elevation: 12,
        shape: RoundedRectangleBorder(
          borderRadius: BorderRadius.circular(4), // Sharp corners for comic feel
          side: const BorderSide(
            color: UrbanColors.comicBlack,
            width: 3, // Thick border like comic panels
          ),
        ),
        margin: const EdgeInsets.all(8),
      ),

      // Button themes - bold and edgy
      elevatedButtonTheme: ElevatedButtonThemeData(
        style: ElevatedButton.styleFrom(
          backgroundColor: UrbanColors.rustOrange,
          foregroundColor: UrbanColors.comicWhite,
          shadowColor: UrbanColors.shadow,
          elevation: 8,
          padding: const EdgeInsets.symmetric(horizontal: 24, vertical: 16),
          shape: RoundedRectangleBorder(
            borderRadius: BorderRadius.circular(4),
            side: const BorderSide(
              color: UrbanColors.comicBlack,
              width: 2,
            ),
          ),
          textStyle: UrbanTextStyles.buttonText,
        ),
      ),

      outlinedButtonTheme: OutlinedButtonThemeData(
        style: OutlinedButton.styleFrom(
          foregroundColor: UrbanColors.neonCyan,
          side: const BorderSide(
            color: UrbanColors.neonCyan,
            width: 2,
          ),
          padding: const EdgeInsets.symmetric(horizontal: 24, vertical: 16),
          shape: RoundedRectangleBorder(
            borderRadius: BorderRadius.circular(4),
          ),
          textStyle: UrbanTextStyles.buttonText,
        ),
      ),

      textButtonTheme: TextButtonThemeData(
        style: TextButton.styleFrom(
          foregroundColor: UrbanColors.rustOrange,
          textStyle: UrbanTextStyles.stencilText,
        ),
      ),

      // Icon theme
      iconTheme: const IconThemeData(
        color: UrbanColors.neonCyan,
        size: 24,
      ),

      // Text theme with urban typography
      textTheme: TextTheme(
        displayLarge: UrbanTextStyles.graffitiDisplay,
        displayMedium: GoogleFonts.bangers(
          fontSize: 36,
          fontWeight: FontWeight.bold,
          letterSpacing: 1.5,
          color: UrbanColors.comicWhite,
        ),
        displaySmall: UrbanTextStyles.streetHeadline,

        headlineLarge: UrbanTextStyles.streetHeadline,
        headlineMedium: UrbanTextStyles.comicTitle,
        headlineSmall: GoogleFonts.permanentMarker(
          fontSize: 20,
          fontWeight: FontWeight.w600,
          letterSpacing: 0.8,
          color: UrbanColors.rustOrange,
        ),

        titleLarge: UrbanTextStyles.comicTitle,
        titleMedium: GoogleFonts.permanentMarker(
          fontSize: 18,
          fontWeight: FontWeight.w600,
          letterSpacing: 0.8,
          color: UrbanColors.comicWhite,
        ),
        titleSmall: UrbanTextStyles.stencilText,

        bodyLarge: UrbanTextStyles.urbanBody,
        bodyMedium: GoogleFonts.robotoCondensed(
          fontSize: 14,
          fontWeight: FontWeight.w500,
          letterSpacing: 0.5,
          color: UrbanColors.moonlight,
        ),
        bodySmall: GoogleFonts.robotoCondensed(
          fontSize: 12,
          fontWeight: FontWeight.w400,
          letterSpacing: 0.4,
          color: UrbanColors.fog,
        ),

        labelLarge: UrbanTextStyles.urbanLabel,
        labelMedium: GoogleFonts.robotoCondensed(
          fontSize: 11,
          fontWeight: FontWeight.w700,
          letterSpacing: 1.2,
          color: UrbanColors.fog,
        ),
        labelSmall: GoogleFonts.robotoCondensed(
          fontSize: 10,
          fontWeight: FontWeight.w700,
          letterSpacing: 1.0,
          color: UrbanColors.graffiti,
        ),
      ),

      // Input decoration theme - urban form style
      inputDecorationTheme: InputDecorationTheme(
        filled: true,
        fillColor: UrbanColors.asphalt,
        border: OutlineInputBorder(
          borderRadius: BorderRadius.circular(4),
          borderSide: const BorderSide(
            color: UrbanColors.graffiti,
            width: 2,
          ),
        ),
        enabledBorder: OutlineInputBorder(
          borderRadius: BorderRadius.circular(4),
          borderSide: const BorderSide(
            color: UrbanColors.graffiti,
            width: 2,
          ),
        ),
        focusedBorder: OutlineInputBorder(
          borderRadius: BorderRadius.circular(4),
          borderSide: const BorderSide(
            color: UrbanColors.neonCyan,
            width: 3,
          ),
        ),
        errorBorder: OutlineInputBorder(
          borderRadius: BorderRadius.circular(4),
          borderSide: const BorderSide(
            color: UrbanColors.dangerRed,
            width: 2,
          ),
        ),
        labelStyle: UrbanTextStyles.urbanLabel,
        hintStyle: TextStyle(
          fontFamily: 'Roboto Condensed',
          fontSize: 14,
          color: UrbanColors.fog,
        ),
      ),

      // Snackbar theme - notification style
      snackBarTheme: SnackBarThemeData(
        backgroundColor: UrbanColors.concreteGray,
        contentTextStyle: UrbanTextStyles.urbanBody,
        shape: RoundedRectangleBorder(
          borderRadius: BorderRadius.circular(4),
          side: const BorderSide(
            color: UrbanColors.neonCyan,
            width: 2,
          ),
        ),
        behavior: SnackBarBehavior.floating,
        elevation: 12,
      ),

      // Dialog theme - comic panel style
      dialogTheme: DialogTheme(
        backgroundColor: UrbanColors.concreteGray,
        elevation: 16,
        shadowColor: UrbanColors.shadow,
        shape: RoundedRectangleBorder(
          borderRadius: BorderRadius.circular(4),
          side: const BorderSide(
            color: UrbanColors.comicBlack,
            width: 4,
          ),
        ),
        titleTextStyle: UrbanTextStyles.comicTitle,
        contentTextStyle: UrbanTextStyles.urbanBody,
      ),

      // Bottom sheet theme
      bottomSheetTheme: const BottomSheetThemeData(
        backgroundColor: UrbanColors.concreteGray,
        elevation: 16,
        shadowColor: UrbanColors.shadow,
        shape: RoundedRectangleBorder(
          borderRadius: BorderRadius.vertical(top: Radius.circular(4)),
          side: BorderSide(
            color: UrbanColors.comicBlack,
            width: 3,
          ),
        ),
      ),

      // Divider theme
      dividerTheme: const DividerThemeData(
        color: UrbanColors.graffiti,
        thickness: 2,
        space: 16,
      ),

      // Chip theme (for tags, etc.)
      chipTheme: ChipThemeData(
        backgroundColor: UrbanColors.asphalt,
        deleteIconColor: UrbanColors.dangerRed,
        disabledColor: UrbanColors.shadow,
        selectedColor: UrbanColors.rustOrange,
        secondarySelectedColor: UrbanColors.neonCyan,
        padding: const EdgeInsets.symmetric(horizontal: 12, vertical: 8),
        labelStyle: UrbanTextStyles.urbanLabel,
        secondaryLabelStyle: UrbanTextStyles.urbanLabel.copyWith(
          color: UrbanColors.comicWhite,
        ),
        brightness: Brightness.dark,
        shape: RoundedRectangleBorder(
          borderRadius: BorderRadius.circular(4),
          side: const BorderSide(
            color: UrbanColors.graffiti,
            width: 2,
          ),
        ),
      ),

      // Switch theme
      switchTheme: SwitchThemeData(
        thumbColor: MaterialStateProperty.resolveWith((states) {
          if (states.contains(MaterialState.selected)) {
            return UrbanColors.neonCyan;
          }
          return UrbanColors.fog;
        }),
        trackColor: MaterialStateProperty.resolveWith((states) {
          if (states.contains(MaterialState.selected)) {
            return UrbanColors.neonCyan.withOpacity(0.5);
          }
          return UrbanColors.graffiti;
        }),
      ),

      // Progress indicator theme
      progressIndicatorTheme: const ProgressIndicatorThemeData(
        color: UrbanColors.neonCyan,
        linearTrackColor: UrbanColors.graffiti,
        circularTrackColor: UrbanColors.graffiti,
      ),
    );
  }
}
