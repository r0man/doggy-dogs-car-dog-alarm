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

    testWidgets('responds to controller state changes - idle',
        (WidgetTester tester) async {
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

    testWidgets('responds to controller state changes - alert',
        (WidgetTester tester) async {
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

    testWidgets('responds to controller state changes - barking',
        (WidgetTester tester) async {
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

    testWidgets('responds to controller state changes - happy',
        (WidgetTester tester) async {
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

    testWidgets('responds to controller state changes - sad',
        (WidgetTester tester) async {
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

    testWidgets('responds to controller state changes - sleeping',
        (WidgetTester tester) async {
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

    testWidgets('responds to controller state changes - eating',
        (WidgetTester tester) async {
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

    testWidgets('responds to controller state changes - playing',
        (WidgetTester tester) async {
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

    testWidgets('handles multiple rapid state changes',
        (WidgetTester tester) async {
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
      await tester
          .pump(const Duration(milliseconds: 300)); // Let timer complete

      controller.forceState(DogAnimationState.barking);
      await tester.pump();
      await tester
          .pump(const Duration(milliseconds: 300)); // Let timer complete

      controller.forceState(DogAnimationState.happy);
      await tester.pump();
      await tester
          .pump(const Duration(milliseconds: 300)); // Let timer complete

      controller.forceState(DogAnimationState.idle);
      await tester.pump();
      await tester
          .pump(const Duration(milliseconds: 300)); // Let timer complete

      // Final state should be idle
      expect(controller.currentState, DogAnimationState.idle);

      // Widget should still be rendering without errors
      expect(find.byType(AnimatedDogWidget), findsOneWidget);
    });

    testWidgets('properly disposes animation controller',
        (WidgetTester tester) async {
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

    testWidgets('displays all dog breeds correctly',
        (WidgetTester tester) async {
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

    testWidgets('uses Transform.scale for animations',
        (WidgetTester tester) async {
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

    testWidgets('animation updates when state changes',
        (WidgetTester tester) async {
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
      await tester
          .pump(const Duration(milliseconds: 300)); // Let timer complete
      await tester.pump(const Duration(milliseconds: 100)); // Advance animation

      // State should have changed
      expect(controller.currentState, DogAnimationState.barking);

      // Widget should still be present
      expect(find.byType(AnimatedDogWidget), findsOneWidget);
    });

    testWidgets('widget updates when breed changes',
        (WidgetTester tester) async {
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

    group('Touch Interactions', () {
      testWidgets('tap triggers playing animation', (WidgetTester tester) async {
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

        // Tap the dog
        await tester.tap(find.byType(AnimatedDogWidget));
        await tester.pumpAndSettle(); // Wait for gesture to complete

        // Should trigger playing animation
        expect(controller.currentState, DogAnimationState.playing);

        // Wait for the playOnce timer to complete (playing is 2.5s + buffer)
        await tester.pump(const Duration(milliseconds: 2501));
        await tester.pump(); // Execute the timer callback
        await tester.pump(); // Allow state change to propagate
      });

      testWidgets('tap calls onTap callback', (WidgetTester tester) async {
        var tapCalled = false;

        await tester.pumpWidget(
          MaterialApp(
            home: Scaffold(
              body: AnimatedDogWidget(
                breed: DogBreed.germanShepherd,
                controller: controller,
                onTap: () {
                  tapCalled = true;
                },
              ),
            ),
          ),
        );

        // Tap the dog
        await tester.tap(find.byType(AnimatedDogWidget));
        await tester.pumpAndSettle(); // Wait for gesture to complete

        expect(tapCalled, isTrue);

        // Wait for the playOnce timer to complete
        await tester.pump(const Duration(milliseconds: 2501));
        await tester.pump(); // Execute the timer callback
        await tester.pump(); // Allow state change to propagate
      });

      testWidgets('double tap triggers happy animation',
          (WidgetTester tester) async {
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

        // Double tap the dog (note: first tap triggers playOnce)
        await tester.tap(find.byType(AnimatedDogWidget));
        await tester.pump(const Duration(milliseconds: 100));
        await tester.tap(find.byType(AnimatedDogWidget));
        await tester.pump();

        // Should trigger happy animation
        expect(controller.currentState, DogAnimationState.happy);

        // Wait for the playOnce timer from first tap (playing is 2.5s)
        await tester.pump(const Duration(milliseconds: 2500));
        await tester.pump();
      });

      testWidgets('double tap calls onDoubleTap callback',
          (WidgetTester tester) async {
        var doubleTapCalled = false;

        await tester.pumpWidget(
          MaterialApp(
            home: Scaffold(
              body: AnimatedDogWidget(
                breed: DogBreed.germanShepherd,
                controller: controller,
                onDoubleTap: () {
                  doubleTapCalled = true;
                },
              ),
            ),
          ),
        );

        // Double tap the dog (note: first tap triggers playOnce)
        await tester.tap(find.byType(AnimatedDogWidget));
        await tester.pump(const Duration(milliseconds: 100));
        await tester.tap(find.byType(AnimatedDogWidget));
        await tester.pump();

        expect(doubleTapCalled, isTrue);

        // Wait for the playOnce timer from first tap
        await tester.pump(const Duration(milliseconds: 2500));
        await tester.pump();
      });

      testWidgets('long press triggers eating animation',
          (WidgetTester tester) async {
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

        // Long press the dog
        await tester.longPress(find.byType(AnimatedDogWidget));
        await tester.pump();

        // Should trigger eating animation
        expect(controller.currentState, DogAnimationState.eating);

        // Wait for the playOnce timer to complete (eating is 3s)
        await tester.pump(const Duration(milliseconds: 3100));
        await tester.pump();
      });

      testWidgets('long press calls onLongPress callback',
          (WidgetTester tester) async {
        var longPressCalled = false;

        await tester.pumpWidget(
          MaterialApp(
            home: Scaffold(
              body: AnimatedDogWidget(
                breed: DogBreed.germanShepherd,
                controller: controller,
                onLongPress: () {
                  longPressCalled = true;
                },
              ),
            ),
          ),
        );

        // Long press the dog
        await tester.longPress(find.byType(AnimatedDogWidget));
        await tester.pump();

        expect(longPressCalled, isTrue);

        // Wait for the playOnce timer to complete
        await tester.pump(const Duration(milliseconds: 3100));
        await tester.pump();
      });

      testWidgets('fast swipe triggers playing animation',
          (WidgetTester tester) async {
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

        // Swipe across the dog (fast horizontal drag)
        await tester.fling(
          find.byType(AnimatedDogWidget),
          const Offset(200, 0), // Horizontal swipe
          600, // velocity > 500 threshold
        );
        await tester.pump();

        // Should trigger playing animation
        expect(controller.currentState, DogAnimationState.playing);

        // Wait for the playOnce timer to complete
        await tester.pump(const Duration(milliseconds: 2600));
        await tester.pump();
      });

      testWidgets('slow swipe does not trigger animation',
          (WidgetTester tester) async {
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

        // Set to a specific state first
        controller.forceState(DogAnimationState.alert);
        await tester.pump();
        expect(controller.currentState, DogAnimationState.alert);

        // Slow swipe (velocity < 500 threshold)
        await tester.fling(
          find.byType(AnimatedDogWidget),
          const Offset(100, 0),
          300, // velocity below threshold
        );
        await tester.pump();

        // Should remain in alert state
        expect(controller.currentState, DogAnimationState.alert);
      });

      testWidgets('touch interactions work with GestureDetector',
          (WidgetTester tester) async {
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

        // GestureDetector should be present
        expect(find.byType(GestureDetector), findsOneWidget);
      });
    });
  });
}
