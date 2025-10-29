import 'package:flutter_riverpod/flutter_riverpod.dart';
import '../services/dog_animation_controller.dart';
import '../services/alarm_service.dart';
import '../models/alarm_state.dart';
import '../models/dog.dart';
import '../models/dog_animation_state.dart';

/// Provider for the dog animation controller
final dogAnimationControllerProvider = Provider<DogAnimationController>((ref) {
  final controller = DogAnimationController();

  // Listen to alarm state changes and update animations
  ref.listen<AsyncValue<AlarmState>>(
    alarmStateProvider,
    (previous, next) {
      next.whenData((alarmState) {
        controller.updateFromAlarmState(alarmState);
      });
    },
  );

  // Dispose when no longer needed
  ref.onDispose(() {
    controller.dispose();
  });

  return controller;
});

/// Provider that exposes the current animation state for easy watching
final currentAnimationStateProvider = Provider((ref) {
  final controller = ref.watch(dogAnimationControllerProvider);
  // This will rebuild when controller notifies
  return controller.currentState;
});

/// Provider to trigger one-time animations (eating, playing)
final playAnimationProvider = Provider((ref) {
  final controller = ref.read(dogAnimationControllerProvider);
  return controller.playOnce;
});

/// Helper to update animation based on dog mood
/// Call this when dog mood changes
void updateAnimationFromMood(WidgetRef ref, DogMood mood) {
  final controller = ref.read(dogAnimationControllerProvider);
  controller.updateFromMood(mood);
}

/// Helper to play a one-time animation
/// Returns a Future that completes when the animation finishes
Future<void> playDogAnimation(
  WidgetRef ref,
  DogAnimationState state,
) async {
  final playAnimation = ref.read(playAnimationProvider);
  await playAnimation(state);
}
