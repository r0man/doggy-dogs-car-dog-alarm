import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'alarm_screen.dart';
import 'breed_selection_screen.dart';
import 'settings_screen.dart';
import 'achievements_screen.dart';
import '../providers/dog_provider.dart';
import '../providers/dog_animation_provider.dart';
import '../providers/dialogue_provider.dart';
import '../providers/achievement_provider.dart';
import '../widgets/animated_dog_widget.dart';
import '../widgets/dialogue_bubble_widget.dart';
import '../models/dog_animation_state.dart';

class HomeScreen extends ConsumerStatefulWidget {
  const HomeScreen({super.key});

  @override
  ConsumerState<HomeScreen> createState() => _HomeScreenState();
}

class _HomeScreenState extends ConsumerState<HomeScreen> {
  @override
  void initState() {
    super.initState();
    // Trigger check-in dialogue when screen loads
    WidgetsBinding.instance.addPostFrameCallback((_) async {
      ref.read(dialogueProvider.notifier).onCheckIn();
      // Track check-in achievement
      await incrementAchievement(ref, 'social_butterfly');
    });
  }

  @override
  Widget build(BuildContext context) {
    final dog = ref.watch(dogProvider);

    if (dog == null) {
      return const Scaffold(
        body: Center(
          child: CircularProgressIndicator(),
        ),
      );
    }

    return Scaffold(
      appBar: AppBar(
        title: Text('🐕 ${dog.name}'),
        centerTitle: true,
        elevation: 0,
        actions: [
          IconButton(
            icon: const Icon(Icons.emoji_events),
            tooltip: 'Achievements',
            onPressed: () {
              Navigator.push(
                context,
                MaterialPageRoute(
                  builder: (context) => const AchievementsScreen(),
                ),
              );
            },
          ),
          IconButton(
            icon: const Icon(Icons.pets),
            tooltip: 'Change Breed',
            onPressed: () {
              Navigator.push(
                context,
                MaterialPageRoute(
                  builder: (context) =>
                      const BreedSelectionScreen(isOnboarding: false),
                ),
              );
            },
          ),
          IconButton(
            icon: const Icon(Icons.settings),
            tooltip: 'Settings',
            onPressed: () {
              Navigator.push(
                context,
                MaterialPageRoute(
                  builder: (context) => const SettingsScreen(),
                ),
              );
            },
          ),
        ],
      ),
      body: SafeArea(
        child: Padding(
          padding: const EdgeInsets.all(16.0),
          child: Column(
            mainAxisAlignment: MainAxisAlignment.center,
            crossAxisAlignment: CrossAxisAlignment.stretch,
            children: [
              // Dog Display Area
              Expanded(
                flex: 2,
                child: Card(
                  child: Padding(
                    padding: const EdgeInsets.all(16.0),
                    child: Column(
                      mainAxisAlignment: MainAxisAlignment.center,
                      children: [
                        // Animated dog with dialogue
                        Expanded(
                          child: Stack(
                            children: [
                              Center(
                                child: AnimatedDogWidget(
                                  breed: dog.breed,
                                  controller: ref.watch(dogAnimationControllerProvider),
                                  size: 200,
                                ),
                              ),
                              // Dialogue bubble above dog
                              const PositionedDialogueBubble(
                                alignment: Alignment.topCenter,
                                margin: EdgeInsets.all(8),
                              ),
                            ],
                          ),
                        ),
                        const SizedBox(height: 16),
                        Text(
                          'Your Guard Dog',
                          style: Theme.of(context).textTheme.headlineSmall,
                        ),
                        const SizedBox(height: 8),
                        Text(
                          'Tap to meet your companion!',
                          style:
                              Theme.of(context).textTheme.bodyMedium?.copyWith(
                                    color: Colors.grey,
                                  ),
                        ),
                      ],
                    ),
                  ),
                ),
              ),
              const SizedBox(height: 16),

              // Status Area
              Card(
                color: Colors.blue.shade50,
                child: Padding(
                  padding: const EdgeInsets.all(16.0),
                  child: Column(
                    children: [
                      Row(
                        mainAxisAlignment: MainAxisAlignment.spaceAround,
                        children: [
                          _buildStatIndicator(
                            icon: Icons.favorite,
                            label: 'Happiness',
                            value: dog.stats.happiness,
                            color: Colors.pink,
                          ),
                          _buildStatIndicator(
                            icon: Icons.restaurant,
                            label: 'Hunger',
                            value: dog.stats.hunger,
                            color: Colors.green,
                          ),
                          _buildStatIndicator(
                            icon: Icons.battery_charging_full,
                            label: 'Energy',
                            value: dog.stats.energy,
                            color: Colors.blue,
                          ),
                        ],
                      ),
                    ],
                  ),
                ),
              ),
              const SizedBox(height: 16),

              // Quick Actions
              Row(
                children: [
                  Expanded(
                    child: ElevatedButton.icon(
                      onPressed: () async {
                        // Trigger eating animation
                        playDogAnimation(ref, DogAnimationState.eating);
                        final messenger = ScaffoldMessenger.of(context);

                        await ref.read(dogProvider.notifier).feed();

                        // Track feeding achievement
                        await incrementAchievement(ref, 'well_fed');

                        // Trigger feeding dialogue
                        ref.read(dialogueProvider.notifier).onFeeding();

                        if (mounted) {
                          messenger.showSnackBar(
                            SnackBar(
                              content: Text('${dog.name} enjoyed the meal!'),
                              backgroundColor: Colors.green,
                            ),
                          );
                        }
                      },
                      icon: const Icon(Icons.restaurant),
                      label: const Text('Feed'),
                      style: ElevatedButton.styleFrom(
                        backgroundColor: Colors.green,
                        foregroundColor: Colors.white,
                      ),
                    ),
                  ),
                  const SizedBox(width: 8),
                  Expanded(
                    child: ElevatedButton.icon(
                      onPressed: () async {
                        // Trigger playing animation
                        playDogAnimation(ref, DogAnimationState.playing);
                        final messenger = ScaffoldMessenger.of(context);

                        await ref.read(dogProvider.notifier).play();

                        // Trigger playing dialogue
                        ref.read(dialogueProvider.notifier).onPlaying();

                        if (mounted) {
                          messenger.showSnackBar(
                            SnackBar(
                              content: Text('${dog.name} had fun playing!'),
                              backgroundColor: Colors.orange,
                            ),
                          );
                        }
                      },
                      icon: const Icon(Icons.sports_tennis),
                      label: const Text('Play'),
                      style: ElevatedButton.styleFrom(
                        backgroundColor: Colors.orange,
                        foregroundColor: Colors.white,
                      ),
                    ),
                  ),
                ],
              ),
              const SizedBox(height: 16),

              // Main Action Button - Alarm
              ElevatedButton(
                onPressed: () {
                  // Trigger alarm activation dialogue
                  ref.read(dialogueProvider.notifier).onAlarmActivated();

                  Navigator.push(
                    context,
                    MaterialPageRoute(
                      builder: (context) => const AlarmScreen(),
                    ),
                  );
                },
                style: ElevatedButton.styleFrom(
                  padding: const EdgeInsets.symmetric(vertical: 20),
                  backgroundColor: Colors.red,
                  foregroundColor: Colors.white,
                ),
                child: const Row(
                  mainAxisAlignment: MainAxisAlignment.center,
                  children: [
                    Icon(Icons.security, size: 28),
                    SizedBox(width: 12),
                    Text(
                      'ACTIVATE GUARD DOG',
                      style: TextStyle(
                        fontSize: 18,
                        fontWeight: FontWeight.bold,
                      ),
                    ),
                  ],
                ),
              ),
            ],
          ),
        ),
      ),
    );
  }

  Widget _buildStatIndicator({
    required IconData icon,
    required String label,
    required int value,
    required Color color,
  }) {
    return Column(
      children: [
        Icon(icon, color: color, size: 32),
        const SizedBox(height: 4),
        Text(
          label,
          style: const TextStyle(fontSize: 12),
        ),
        const SizedBox(height: 4),
        Text(
          '$value%',
          style: TextStyle(
            fontSize: 16,
            fontWeight: FontWeight.bold,
            color: color,
          ),
        ),
      ],
    );
  }
}
