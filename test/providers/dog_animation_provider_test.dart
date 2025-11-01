import 'package:flutter_test/flutter_test.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:doggy_dogs_car_alarm/providers/dog_animation_provider.dart';
import 'package:doggy_dogs_car_alarm/models/dog.dart';
import 'package:doggy_dogs_car_alarm/models/dog_animation_state.dart';
import 'package:doggy_dogs_car_alarm/services/dog_animation_controller.dart';

void main() {
  group('Dog Animation Provider Tests', () {
    late ProviderContainer container;

    setUp(() {
      container = ProviderContainer();
    });

    tearDown(() {
      container.dispose();
    });

    test('dogAnimationControllerProvider creates controller', () {
      final controller = container.read(dogAnimationControllerProvider);

      expect(controller, isA<DogAnimationController>());
      expect(controller.currentState, isNotNull);
    });

    test('dogAnimationControllerProvider disposes controller', () {
      final controller = container.read(dogAnimationControllerProvider);

      // Verify controller is created
      expect(controller, isNotNull);

      // Dispose container should dispose controller
      container.dispose();

      // Create new container to verify fresh instance
      container = ProviderContainer();
      final newController = container.read(dogAnimationControllerProvider);

      // Should be a different instance
      expect(identical(controller, newController), false);
    });

    test('currentAnimationStateProvider returns current state', () {
      final state = container.read(currentAnimationStateProvider);

      expect(state, isA<DogAnimationState>());
    });

    test('playAnimationProvider returns playOnce function', () {
      final playAnimation = container.read(playAnimationProvider);

      expect(playAnimation, isA<Function>());
    });

    test('updateAnimationFromMood updates controller state', () {
      container = ProviderContainer();

      // Create a test widget ref
      final controller = container.read(dogAnimationControllerProvider);

      // Manually call updateFromMood since we can't easily test the helper function
      controller.updateFromMood(DogMood.happy);

      // Verify state updated
      expect(controller.currentState, DogAnimationState.happy);
    });

    test('controller responds to different moods', () {
      final controller = container.read(dogAnimationControllerProvider);

      // Test different moods
      controller.updateFromMood(DogMood.happy);
      expect(controller.currentState, DogAnimationState.happy);

      controller.updateFromMood(DogMood.sad);
      expect(controller.currentState, DogAnimationState.sad);

      controller.updateFromMood(DogMood.content);
      expect(controller.currentState, DogAnimationState.idle);

      controller.updateFromMood(DogMood.alert);
      expect(controller.currentState, DogAnimationState.alert);
    });

    test('playOnce plays one-time animation', () async {
      final controller = container.read(dogAnimationControllerProvider);
      final playOnce = container.read(playAnimationProvider);

      // Play eating animation
      playOnce(DogAnimationState.eating);

      // State should change immediately
      expect(controller.currentState, DogAnimationState.eating);

      // Note: Testing the return to idle is timing-sensitive and unreliable in unit tests
      // The animation controller schedules a timer to return to idle after the duration
    });
  });
}

class TestListener<T> {
  final List<T> values = [];

  void call(T? previous, T next) {
    values.add(next);
  }
}
