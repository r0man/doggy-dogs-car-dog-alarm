import 'package:flutter/material.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:doggy_dogs_car_alarm/screens/breed_selection_screen.dart';
import 'package:shared_preferences/shared_preferences.dart';

void main() {
  group('BreedSelectionScreen Tests', () {
    setUp(() async {
      SharedPreferences.setMockInitialValues({});
    });

    testWidgets('creates screen in onboarding mode', (tester) async {
      await tester.pumpWidget(
        const ProviderScope(
          child: MaterialApp(
            home: BreedSelectionScreen(isOnboarding: true),
          ),
        ),
      );

      await tester.pumpAndSettle();

      expect(find.byType(BreedSelectionScreen), findsOneWidget);
      expect(find.byType(GridView), findsOneWidget);
    });

    testWidgets('creates screen in non-onboarding mode', (tester) async {
      await tester.pumpWidget(
        const ProviderScope(
          child: MaterialApp(
            home: BreedSelectionScreen(isOnboarding: false),
          ),
        ),
      );

      await tester.pumpAndSettle();

      expect(find.byType(BreedSelectionScreen), findsOneWidget);
    });

    testWidgets('displays all dog breeds', (tester) async {
      await tester.pumpWidget(
        const ProviderScope(
          child: MaterialApp(
            home: BreedSelectionScreen(),
          ),
        ),
      );

      await tester.pumpAndSettle();

      // Should have a card for each breed
      expect(find.byType(Card), findsWidgets);
    });

    testWidgets('can select a breed', (tester) async {
      await tester.pumpWidget(
        const ProviderScope(
          child: MaterialApp(
            home: BreedSelectionScreen(),
          ),
        ),
      );

      await tester.pumpAndSettle();

      // Tap the first breed card
      final cards = find.byType(GestureDetector);
      expect(cards, findsWidgets);

      await tester.tap(cards.first);
      await tester.pump();

      // Breed should be selected (state change)
      expect(find.byType(BreedSelectionScreen), findsOneWidget);
    });

    testWidgets('displays breed information', (tester) async {
      await tester.pumpWidget(
        const ProviderScope(
          child: MaterialApp(
            home: BreedSelectionScreen(),
          ),
        ),
      );

      await tester.pumpAndSettle();

      // Should display GridView for breeds
      expect(find.byType(GridView), findsOneWidget);

      // Should have at least some breed cards (GridView.builder is lazy)
      expect(find.byType(Card), findsWidgets);
    });

    testWidgets('has scaffold and safe area', (tester) async {
      await tester.pumpWidget(
        const ProviderScope(
          child: MaterialApp(
            home: BreedSelectionScreen(),
          ),
        ),
      );

      await tester.pumpAndSettle();

      expect(find.byType(Scaffold), findsOneWidget);
      expect(find.byType(SafeArea), findsWidgets);
    });

    testWidgets('displays text field for dog name', (tester) async {
      await tester.pumpWidget(
        const ProviderScope(
          child: MaterialApp(
            home: BreedSelectionScreen(isOnboarding: true),
          ),
        ),
      );

      await tester.pumpAndSettle();

      // TextField only appears after selecting a breed
      final cards = find.byType(GestureDetector);
      expect(cards, findsWidgets);

      // Select the first breed
      await tester.tap(cards.first);
      await tester.pump();

      // Now the TextField should appear
      expect(find.byType(TextField), findsOneWidget);
    });

    testWidgets('shows onboarding welcome text', (tester) async {
      await tester.pumpWidget(
        const ProviderScope(
          child: MaterialApp(
            home: BreedSelectionScreen(isOnboarding: true),
          ),
        ),
      );

      await tester.pumpAndSettle();

      expect(find.textContaining('Welcome'), findsOneWidget);
    });

    testWidgets('does not show back button when onboarding', (tester) async {
      await tester.pumpWidget(
        const ProviderScope(
          child: MaterialApp(
            home: BreedSelectionScreen(isOnboarding: true),
          ),
        ),
      );

      await tester.pumpAndSettle();

      final appBar = tester.widget<AppBar>(find.byType(AppBar));
      expect(appBar.automaticallyImplyLeading, false);
    });

    testWidgets('shows back button when not onboarding', (tester) async {
      await tester.pumpWidget(
        const ProviderScope(
          child: MaterialApp(
            home: BreedSelectionScreen(isOnboarding: false),
          ),
        ),
      );

      await tester.pumpAndSettle();

      final appBar = tester.widget<AppBar>(find.byType(AppBar));
      expect(appBar.automaticallyImplyLeading, true);
    });
  });
}
