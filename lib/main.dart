import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:shared_preferences/shared_preferences.dart';
import 'screens/home_screen.dart';
import 'screens/breed_selection_screen.dart';
import 'providers/dog_provider.dart';
import 'services/unlock_code_service.dart';

void main() async {
  WidgetsFlutterBinding.ensureInitialized();

  // Initialize SharedPreferences
  final sharedPreferences = await SharedPreferences.getInstance();

  runApp(
    ProviderScope(
      overrides: [
        // Override SharedPreferences provider with initialized instance
        sharedPreferencesProvider.overrideWithValue(sharedPreferences),
      ],
      child: const DoggyDogsCarAlarmApp(),
    ),
  );
}

class DoggyDogsCarAlarmApp extends ConsumerWidget {
  const DoggyDogsCarAlarmApp({super.key});

  @override
  Widget build(BuildContext context, WidgetRef ref) {
    return MaterialApp(
      title: 'Doggy Dogs Car Alarm',
      theme: ThemeData(
        // Manga-inspired color scheme
        colorScheme: ColorScheme.fromSeed(
          seedColor: Colors.orange,
          brightness: Brightness.light,
        ),
        useMaterial3: true,
        fontFamily: 'Roboto',

        // Friendly, rounded components
        cardTheme: CardThemeData(
          elevation: 4,
          shape: RoundedRectangleBorder(
            borderRadius: BorderRadius.circular(16),
          ),
        ),

        elevatedButtonTheme: ElevatedButtonThemeData(
          style: ElevatedButton.styleFrom(
            padding: const EdgeInsets.symmetric(horizontal: 32, vertical: 16),
            shape: RoundedRectangleBorder(
              borderRadius: BorderRadius.circular(12),
            ),
          ),
        ),
      ),
      home: const AppStartupScreen(),
      debugShowCheckedModeBanner: false,
    );
  }
}

/// Screen that determines whether to show onboarding or home
class AppStartupScreen extends ConsumerWidget {
  const AppStartupScreen({super.key});

  @override
  Widget build(BuildContext context, WidgetRef ref) {
    final dog = ref.watch(dogProvider);

    // Show loading while dog provider initializes
    if (dog == null) {
      // Check if we're still loading or if there's no dog
      return FutureBuilder(
        future: Future.delayed(const Duration(milliseconds: 500)),
        builder: (context, snapshot) {
          if (snapshot.connectionState == ConnectionState.waiting) {
            return const Scaffold(
              body: Center(
                child: CircularProgressIndicator(),
              ),
            );
          }

          // After initial load, check again
          final currentDog = ref.read(dogProvider);
          if (currentDog == null) {
            // No dog - show onboarding
            return const BreedSelectionScreen(isOnboarding: true);
          } else {
            // Has dog - show home
            return const HomeScreen();
          }
        },
      );
    }

    // Has dog - show home screen
    return const HomeScreen();
  }
}
