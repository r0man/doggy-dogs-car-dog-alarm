/// Animation states for the virtual dog
enum DogAnimationState {
  /// Default resting state - gentle breathing, occasional ear twitch
  idle,

  /// Vigilant watching - ears up, eyes focused, ready to respond
  alert,

  /// Active alarm - barking with full body animation
  barking,

  /// Joyful state - tail wagging, happy expression
  happy,

  /// Low energy state - droopy ears, slow movement
  sad,

  /// Resting state - eyes closed, slow breathing cycle
  sleeping,

  /// Eating animation - head down, munching
  eating,

  /// Playful bouncing and movement
  playing,
}

extension DogAnimationStateExtension on DogAnimationState {
  /// Display name for this animation state
  String get displayName {
    switch (this) {
      case DogAnimationState.idle:
        return 'Idle';
      case DogAnimationState.alert:
        return 'Alert';
      case DogAnimationState.barking:
        return 'Barking';
      case DogAnimationState.happy:
        return 'Happy';
      case DogAnimationState.sad:
        return 'Sad';
      case DogAnimationState.sleeping:
        return 'Sleeping';
      case DogAnimationState.eating:
        return 'Eating';
      case DogAnimationState.playing:
        return 'Playing';
    }
  }

  /// Description of what this animation represents
  String get description {
    switch (this) {
      case DogAnimationState.idle:
        return 'Calm and relaxed, waiting patiently';
      case DogAnimationState.alert:
        return 'Watching carefully, ready to protect';
      case DogAnimationState.barking:
        return 'Actively barking to scare intruders';
      case DogAnimationState.happy:
        return 'Excited and joyful, tail wagging';
      case DogAnimationState.sad:
        return 'Feeling down, needs attention';
      case DogAnimationState.sleeping:
        return 'Peacefully resting and recharging';
      case DogAnimationState.eating:
        return 'Enjoying a tasty treat';
      case DogAnimationState.playing:
        return 'Full of energy and ready to play';
    }
  }

  /// Whether this animation should loop continuously
  bool get isLooping {
    switch (this) {
      case DogAnimationState.idle:
      case DogAnimationState.alert:
      case DogAnimationState.barking:
      case DogAnimationState.happy:
      case DogAnimationState.sad:
      case DogAnimationState.sleeping:
        return true;
      case DogAnimationState.eating:
      case DogAnimationState.playing:
        return false; // Play once, then return to idle
    }
  }

  /// Expected duration in seconds (for non-looping animations)
  double get duration {
    switch (this) {
      case DogAnimationState.eating:
        return 3.0;
      case DogAnimationState.playing:
        return 2.5;
      default:
        return 0.0; // Looping animations don't have fixed duration
    }
  }

  /// Priority level for state transitions (higher = more important)
  /// Used when multiple state changes are requested
  int get priority {
    switch (this) {
      case DogAnimationState.barking:
        return 100; // Alarm is highest priority
      case DogAnimationState.alert:
        return 90;
      case DogAnimationState.sleeping:
        return 10; // Can be interrupted easily
      case DogAnimationState.eating:
        return 60;
      case DogAnimationState.playing:
        return 50;
      case DogAnimationState.happy:
        return 40;
      case DogAnimationState.sad:
        return 40;
      case DogAnimationState.idle:
        return 20;
    }
  }
}
