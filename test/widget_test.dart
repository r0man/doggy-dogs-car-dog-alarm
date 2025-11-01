import 'package:flutter/material.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:shared_preferences/shared_preferences.dart';

import 'package:doggy_dogs_car_alarm/main.dart';
import 'package:doggy_dogs_car_alarm/services/unlock_code_service.dart';

void main() {
  testWidgets('App loads correctly', (WidgetTester tester) async {
    // Initialize SharedPreferences for testing
    SharedPreferences.setMockInitialValues({});
    final sharedPreferences = await SharedPreferences.getInstance();

    // Build our app and trigger a frame.
    await tester.pumpWidget(
      ProviderScope(
        overrides: [
          sharedPreferencesProvider.overrideWithValue(sharedPreferences),
        ],
        child: const DoggyDogsCarAlarmApp(),
      ),
    );

    // Wait for the app to settle
    await tester.pumpAndSettle();

    // Verify the app loads without crashing
    expect(find.byType(MaterialApp), findsOneWidget);
  });
}
