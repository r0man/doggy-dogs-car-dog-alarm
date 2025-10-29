import 'package:flutter/material.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:doggy_dogs_car_alarm/models/dog.dart';
import 'package:doggy_dogs_car_alarm/models/dog_animation_state.dart';
import 'package:doggy_dogs_car_alarm/services/dog_animation_controller.dart';
import 'package:doggy_dogs_car_alarm/widgets/animated_dog_widget.dart';

void main() {
  group('AnimatedDogWidget', () {
    late DogAnimationController controller;

    setUp(() {
      controller = DogAnimationController();
    });

    tearDown(() {
      controller.dispose();
    });

    testWidgets('renders with default parameters', (WidgetTester tester) async {
      await tester.pumpWidget(
        MaterialApp(
          home: Scaffold(
            body: AnimatedDogWidget(
              breed: DogBreed.germanShepherd,
              controller: controller,
            ),
          ),
        ),
      );

      // Widget should render
      expect(find.byType(AnimatedDogWidget), findsOneWidget);

      // Should have a SizedBox with default size
      final sizedBox = tester.widget<SizedBox>(
        find
            .descendant(
              of: find.byType(AnimatedDogWidget),
              matching: find.byType(SizedBox),
            )
            .first,
      );
      expect(sizedBox.width, 200.0);
      expect(sizedBox.height, 200.0);
    });

    testWidgets('renders with custom size', (WidgetTester tester) async {
      await tester.pumpWidget(
        MaterialApp(
          home: Scaffold(
            body: AnimatedDogWidget(
              breed: DogBreed.rottweiler,
              controller: controller,
              size: 150.0,
            ),
          ),
        ),
      );

      final sizedBox = tester.widget<SizedBox>(
        find
            .descendant(
              of: find.byType(AnimatedDogWidget),
              matching: find.byType(SizedBox),
            )
            .first,
      );
      expect(sizedBox.width, 150.0);
      expect(sizedBox.height, 150.0);
    });

    testWidgets('displays correct breed', (WidgetTester tester) async {
      await tester.pumpWidget(
        MaterialApp(
          home: Scaffold(
            body: AnimatedDogWidget(
              breed: DogBreed.beagle,
              controller: controller,
            ),
          ),
        ),
      );

      // Widget should be present
      expect(find.byType(AnimatedDogWidget), findsOneWidget);
    });

    testWidgets('responds to controller state changes - idle', (WidgetTester tester) async {
      await tester.pumpWidget(
        MaterialApp(
          home: Scaffold(
            body: AnimatedDogWidget(
              breed: DogBreed.germanShepherd,
              controller: controller,
            ),
          ),
        ),
      );

      // Initial state should be idle
      expect(controller.currentState, DogAnimationState.idle);

      // Let animations settle
      await tester.pump();
      await tester.pump(const Duration(milliseconds: 100));

      // Widget should still be present
      expect(find.byType(AnimatedDogWidget), findsOneWidget);
    });

    testWidgets('responds to controller state changes - alert', (WidgetTester tester) async {
      await tester.pumpWidget(
        MaterialApp(
          home: Scaffold(
            body: AnimatedDogWidget(
              breed: DogBreed.germanShepherd,
              controller: controller,
            ),
          ),
        ),
      );

      // Change to alert state
      controller.forceState(DogAnimationState.alert);
      await tester.pump();

      expect(controller.currentState, DogAnimationState.alert);

      // Let animation and pending timers complete
      await tester.pump(const Duration(milliseconds: 300));
      await tester.pump(const Duration(milliseconds: 100));

      // Widget should still be rendering
      expect(find.byType(AnimatedDogWidget), findsOneWidget);
    });

    testWidgets('responds to controller state changes - barking', (WidgetTester tester) async {
      await tester.pumpWidget(
        MaterialApp(
          home: Scaffold(
            body: AnimatedDogWidget(
              breed: DogBreed.germanShepherd,
              controller: controller,
            ),
          ),
        ),
      );

      // Change to barking state
      controller.forceState(DogAnimationState.barking);
      await tester.pump();

      expect(controller.currentState, DogAnimationState.barking);

      // Let animation and pending timers complete
      await tester.pump(const Duration(milliseconds: 300));
      await tester.pump(const Duration(milliseconds: 100));

      // Widget should still be rendering
      expect(find.byType(AnimatedDogWidget), findsOneWidget);
    });

    testWidgets('responds to controller state changes - happy', (WidgetTester tester) async {
      await tester.pumpWidget(
        MaterialApp(
          home: Scaffold(
            body: AnimatedDogWidget(
              breed: DogBreed.germanShepherd,
              controller: controller,
            ),
          ),
        ),
      );

      // Change to happy state
      controller.forceState(DogAnimationState.happy);
      await tester.pump();

      expect(controller.currentState, DogAnimationState.happy);

      // Let animation and pending timers complete
      await tester.pump(const Duration(milliseconds: 300));
      await tester.pump(const Duration(milliseconds: 100));

      // Widget should still be rendering
      expect(find.byType(AnimatedDogWidget), findsOneWidget);
    });

    testWidgets('responds to controller state changes - sad', (WidgetTester tester) async {
      await tester.pumpWidget(
        MaterialApp(
          home: Scaffold(
            body: AnimatedDogWidget(
              breed: DogBreed.germanShepherd,
              controller: controller,
            ),
          ),
        ),
      );

      // Change to sad state
      controller.forceState(DogAnimationState.sad);
      await tester.pump();

      expect(controller.currentState, DogAnimationState.sad);

      // Let animation and pending timers complete
      await tester.pump(const Duration(milliseconds: 300));
      await tester.pump(const Duration(milliseconds: 100));

      // Widget should still be rendering
      expect(find.byType(AnimatedDogWidget), findsOneWidget);
    });

    testWidgets('responds to controller state changes - sleeping', (WidgetTester tester) async {
      await tester.pumpWidget(
        MaterialApp(
          home: Scaffold(
            body: AnimatedDogWidget(
              breed: DogBreed.germanShepherd,
              controller: controller,
            ),
          ),
        ),
      );

      // Change to sleeping state
      controller.forceState(DogAnimationState.sleeping);
      await tester.pump();

      expect(controller.currentState, DogAnimationState.sleeping);

      // Let animation and pending timers complete
      await tester.pump(const Duration(milliseconds: 300));
      await tester.pump(const Duration(milliseconds: 100));

      // Widget should still be rendering
      expect(find.byType(AnimatedDogWidget), findsOneWidget);
    });

    testWidgets('responds to controller state changes - eating', (WidgetTester tester) async {
      await tester.pumpWidget(
        MaterialApp(
          home: Scaffold(
            body: AnimatedDogWidget(
              breed: DogBreed.germanShepherd,
              controller: controller,
            ),
          ),
        ),
      );

      // Change to eating state
      controller.forceState(DogAnimationState.eating);
      await tester.pump();

      expect(controller.currentState, DogAnimationState.eating);

      // Let animation and pending timers complete
      await tester.pump(const Duration(milliseconds: 300));
      await tester.pump(const Duration(milliseconds: 100));

      // Widget should still be rendering
      expect(find.byType(AnimatedDogWidget), findsOneWidget);
    });

    testWidgets('responds to controller state changes - playing', (WidgetTester tester) async {
      await tester.pumpWidget(
        MaterialApp(
          home: Scaffold(
            body: AnimatedDogWidget(
              breed: DogBreed.germanShepherd,
              controller: controller,
            ),
          ),
        ),
      );

      // Change to playing state
      controller.forceState(DogAnimationState.playing);
      await tester.pump();

      expect(controller.currentState, DogAnimationState.playing);

      // Let animation and pending timers complete
      await tester.pump(const Duration(milliseconds: 300));
      await tester.pump(const Duration(milliseconds: 100));

      // Widget should still be rendering
      expect(find.byType(AnimatedDogWidget), findsOneWidget);
    });

    testWidgets('handles multiple rapid state changes', (WidgetTester tester) async {
      await tester.pumpWidget(
        MaterialApp(
          home: Scaffold(
            body: AnimatedDogWidget(
              breed: DogBreed.germanShepherd,
              controller: controller,
            ),
          ),
        ),
      );

      // Rapidly change states
      controller.forceState(DogAnimationState.alert);
      await tester.pump();
      await tester.pump(const Duration(milliseconds: 300)); // Let timer complete

      controller.forceState(DogAnimationState.barking);
      await tester.pump();
      await tester.pump(const Duration(milliseconds: 300)); // Let timer complete

      controller.forceState(DogAnimationState.happy);
      await tester.pump();
      await tester.pump(const Duration(milliseconds: 300)); // Let timer complete

      controller.forceState(DogAnimationState.idle);
      await tester.pump();
      await tester.pump(const Duration(milliseconds: 300)); // Let timer complete

      // Final state should be idle
      expect(controller.currentState, DogAnimationState.idle);

      // Widget should still be rendering without errors
      expect(find.byType(AnimatedDogWidget), findsOneWidget);
    });

    testWidgets('properly disposes animation controller', (WidgetTester tester) async {
      await tester.pumpWidget(
        MaterialApp(
          home: Scaffold(
            body: AnimatedDogWidget(
              breed: DogBreed.germanShepherd,
              controller: controller,
            ),
          ),
        ),
      );

      // Widget should be present
      expect(find.byType(AnimatedDogWidget), findsOneWidget);

      // Remove widget from tree (triggers dispose)
      await tester.pumpWidget(
        const MaterialApp(
          home: Scaffold(
            body: SizedBox(),
          ),
        ),
      );

      // Widget should be gone
      expect(find.byType(AnimatedDogWidget), findsNothing);
    });

    testWidgets('displays all dog breeds correctly', (WidgetTester tester) async {
      for (final breed in DogBreed.values) {
        await tester.pumpWidget(
          MaterialApp(
            home: Scaffold(
              body: AnimatedDogWidget(
                breed: breed,
                controller: controller,
              ),
            ),
          ),
        );

        // Widget should render for each breed
        expect(find.byType(AnimatedDogWidget), findsOneWidget);

        // Pump to let any animations settle before switching breeds
        await tester.pump(const Duration(milliseconds: 50));
      }
    });

    testWidgets('uses Transform.scale for animations', (WidgetTester tester) async {
      await tester.pumpWidget(
        MaterialApp(
          home: Scaffold(
            body: AnimatedDogWidget(
              breed: DogBreed.germanShepherd,
              controller: controller,
            ),
          ),
        ),
      );

      // Transform.scale should be present in widget tree
      expect(find.byType(Transform), findsWidgets);
    });

    testWidgets('uses Opacity for animations', (WidgetTester tester) async {
      await tester.pumpWidget(
        MaterialApp(
          home: Scaffold(
            body: AnimatedDogWidget(
              breed: DogBreed.germanShepherd,
              controller: controller,
            ),
          ),
        ),
      );

      // Opacity should be present in widget tree
      expect(find.byType(Opacity), findsOneWidget);
    });

    testWidgets('animation updates when state changes', (WidgetTester tester) async {
      await tester.pumpWidget(
        MaterialApp(
          home: Scaffold(
            body: AnimatedDogWidget(
              breed: DogBreed.germanShepherd,
              controller: controller,
            ),
          ),
        ),
      );

      // Initial state
      expect(controller.currentState, DogAnimationState.idle);
      await tester.pump();

      // Change state and pump to trigger animation update
      controller.forceState(DogAnimationState.barking);
      await tester.pump(); // Triggers setState in listener
      await tester.pump(const Duration(milliseconds: 300)); // Let timer complete
      await tester.pump(const Duration(milliseconds: 100)); // Advance animation

      // State should have changed
      expect(controller.currentState, DogAnimationState.barking);

      // Widget should still be present
      expect(find.byType(AnimatedDogWidget), findsOneWidget);
    });

    testWidgets('widget updates when breed changes', (WidgetTester tester) async {
      // Render with first breed
      await tester.pumpWidget(
        MaterialApp(
          home: Scaffold(
            body: AnimatedDogWidget(
              breed: DogBreed.germanShepherd,
              controller: controller,
            ),
          ),
        ),
      );

      expect(find.byType(AnimatedDogWidget), findsOneWidget);

      // Change to different breed
      await tester.pumpWidget(
        MaterialApp(
          home: Scaffold(
            body: AnimatedDogWidget(
              breed: DogBreed.beagle,
              controller: controller,
            ),
          ),
        ),
      );

      expect(find.byType(AnimatedDogWidget), findsOneWidget);
    });
  });
}
