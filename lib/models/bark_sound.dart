import 'dog.dart';

/// Represents a bark sound with characteristics
class BarkSound {
  final BarkType type;
  final BarkIntensity intensity;
  final DogBreed breed;

  const BarkSound({
    required this.type,
    required this.intensity,
    required this.breed,
  });

  /// Get asset path for this bark sound
  /// Note: AssetSource automatically adds "assets/" prefix, so we omit it here
  String get assetPath {
    final breedName = breed.name.toLowerCase();
    final typeName = type.name;
    final intensityName = intensity.name;
    return 'sounds/$breedName/${typeName}_$intensityName.mp3';
  }

  @override
  String toString() => 'BarkSound($breed, $type, $intensity)';
}

/// Type of bark sound
enum BarkType {
  warning, // Single warning bark
  alert, // Alert sequence
  aggressive, // Aggressive barking
  threat, // Serious threat
}

extension BarkTypeExtension on BarkType {
  String get displayName {
    switch (this) {
      case BarkType.warning:
        return 'Warning';
      case BarkType.alert:
        return 'Alert';
      case BarkType.aggressive:
        return 'Aggressive';
      case BarkType.threat:
        return 'Threat';
    }
  }

  String get description {
    switch (this) {
      case BarkType.warning:
        return 'Single warning bark to deter minor disturbances';
      case BarkType.alert:
        return 'Alert barking sequence for suspicious activity';
      case BarkType.aggressive:
        return 'Aggressive barking for clear threats';
      case BarkType.threat:
        return 'Intense barking for serious threats';
    }
  }

  /// Duration in seconds for this bark type
  /// NOTE: All current audio files are normalized to 3.0 seconds
  /// Future enhancement: Add duration-specific variants (warning=1s, alert=3s, aggressive=5s, threat=8s)
  double get duration {
    // All sounds currently normalized to 3 seconds
    // TODO: Implement duration-specific variants per bark type
    return 3.0;

    // Future implementation:
    // switch (this) {
    //   case BarkType.warning: return 1.0;
    //   case BarkType.alert: return 3.0;
    //   case BarkType.aggressive: return 5.0;
    //   case BarkType.threat: return 8.0;
    // }
  }
}

/// Intensity level of the bark
enum BarkIntensity {
  low,
  medium,
  high,
  maximum,
}

extension BarkIntensityExtension on BarkIntensity {
  String get displayName {
    switch (this) {
      case BarkIntensity.low:
        return 'Low';
      case BarkIntensity.medium:
        return 'Medium';
      case BarkIntensity.high:
        return 'High';
      case BarkIntensity.maximum:
        return 'Maximum';
    }
  }

  /// Volume multiplier (0.0 to 1.0)
  double get volume {
    switch (this) {
      case BarkIntensity.low:
        return 0.4;
      case BarkIntensity.medium:
        return 0.6;
      case BarkIntensity.high:
        return 0.8;
      case BarkIntensity.maximum:
        return 1.0;
    }
  }
}

/// Escalation pattern for barking
class BarkEscalation {
  final List<BarkLevel> levels;
  final Duration timeBetweenLevels;
  int currentLevel;

  BarkEscalation({
    required this.levels,
    this.timeBetweenLevels = const Duration(seconds: 5),
    this.currentLevel = 0,
  });

  /// Get current bark level
  BarkLevel get current {
    if (currentLevel >= levels.length) {
      return levels.last;
    }
    return levels[currentLevel];
  }

  /// Escalate to next level
  bool escalate() {
    if (currentLevel < levels.length - 1) {
      currentLevel++;
      return true;
    }
    return false; // Already at max level
  }

  /// Reset to first level
  void reset() {
    currentLevel = 0;
  }

  /// Check if at maximum escalation
  bool get isMaxLevel => currentLevel >= levels.length - 1;

  /// Predefined escalation patterns
  static BarkEscalation get standard => BarkEscalation(
        levels: [
          const BarkLevel(type: BarkType.warning, intensity: BarkIntensity.low),
          const BarkLevel(
              type: BarkType.alert, intensity: BarkIntensity.medium),
          const BarkLevel(
              type: BarkType.aggressive, intensity: BarkIntensity.high),
          const BarkLevel(
              type: BarkType.threat, intensity: BarkIntensity.maximum),
        ],
      );

  static BarkEscalation get aggressive => BarkEscalation(
        levels: [
          const BarkLevel(type: BarkType.alert, intensity: BarkIntensity.high),
          const BarkLevel(
              type: BarkType.aggressive, intensity: BarkIntensity.maximum),
          const BarkLevel(
              type: BarkType.threat, intensity: BarkIntensity.maximum),
        ],
        timeBetweenLevels: const Duration(seconds: 3),
      );

  static BarkEscalation get stealth => BarkEscalation(
        levels: [
          // No actual barking in stealth mode, but keep structure
          const BarkLevel(type: BarkType.warning, intensity: BarkIntensity.low),
        ],
      );
}

/// Represents a level in the escalation pattern
class BarkLevel {
  final BarkType type;
  final BarkIntensity intensity;
  final int repeatCount; // How many times to repeat at this level

  const BarkLevel({
    required this.type,
    required this.intensity,
    this.repeatCount = 1,
  });

  BarkSound toBarkSound(DogBreed breed) {
    return BarkSound(
      type: type,
      intensity: intensity,
      breed: breed,
    );
  }

  @override
  String toString() => 'BarkLevel($type, $intensity, x$repeatCount)';
}
