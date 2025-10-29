import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:flutter_svg/flutter_svg.dart';
import '../models/dog.dart';
import '../providers/dog_provider.dart';

class BreedSelectionScreen extends ConsumerStatefulWidget {
  final bool isOnboarding;

  const BreedSelectionScreen({
    super.key,
    this.isOnboarding = false,
  });

  @override
  ConsumerState<BreedSelectionScreen> createState() =>
      _BreedSelectionScreenState();
}

class _BreedSelectionScreenState extends ConsumerState<BreedSelectionScreen> {
  DogBreed? _selectedBreed;
  final _nameController = TextEditingController();

  @override
  void dispose() {
    _nameController.dispose();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: Text(
            widget.isOnboarding ? 'Choose Your Guard Dog' : 'Change Breed'),
        centerTitle: true,
        automaticallyImplyLeading: !widget.isOnboarding,
      ),
      body: SafeArea(
        child: Padding(
          padding: const EdgeInsets.all(16.0),
          child: Column(
            crossAxisAlignment: CrossAxisAlignment.stretch,
            children: [
              if (widget.isOnboarding) ...[
                Text(
                  'Welcome to Doggy Dogs Car Alarm!',
                  style: Theme.of(context).textTheme.headlineSmall?.copyWith(
                        fontWeight: FontWeight.bold,
                      ),
                  textAlign: TextAlign.center,
                ),
                const SizedBox(height: 8),
                Text(
                  'Choose a loyal guard dog to protect your car',
                  style: Theme.of(context).textTheme.bodyLarge?.copyWith(
                        color: Colors.grey.shade600,
                      ),
                  textAlign: TextAlign.center,
                ),
                const SizedBox(height: 24),
              ],

              // Breed Selection Grid
              Expanded(
                child: GridView.builder(
                  gridDelegate: const SliverGridDelegateWithFixedCrossAxisCount(
                    crossAxisCount: 2,
                    childAspectRatio: 0.85,
                    crossAxisSpacing: 12,
                    mainAxisSpacing: 12,
                  ),
                  itemCount: DogBreed.values.length,
                  itemBuilder: (context, index) {
                    final breed = DogBreed.values[index];
                    final isSelected = _selectedBreed == breed;

                    return GestureDetector(
                      onTap: () {
                        setState(() {
                          _selectedBreed = breed;
                        });
                      },
                      child: Card(
                        elevation: isSelected ? 8 : 2,
                        color: isSelected
                            ? Theme.of(context).colorScheme.primaryContainer
                            : null,
                        shape: RoundedRectangleBorder(
                          borderRadius: BorderRadius.circular(12),
                          side: BorderSide(
                            color: isSelected
                                ? Theme.of(context).colorScheme.primary
                                : Colors.transparent,
                            width: 2,
                          ),
                        ),
                        child: Padding(
                          padding: const EdgeInsets.all(12.0),
                          child: Column(
                            mainAxisAlignment: MainAxisAlignment.center,
                            children: [
                              // Dog Image
                              Expanded(
                                child: SvgPicture.asset(
                                  breed.assetPath,
                                  fit: BoxFit.contain,
                                ),
                              ),
                              const SizedBox(height: 8),
                              // Breed Name
                              Text(
                                breed.displayName,
                                style: Theme.of(context)
                                    .textTheme
                                    .titleMedium
                                    ?.copyWith(
                                      fontWeight: isSelected
                                          ? FontWeight.bold
                                          : FontWeight.normal,
                                    ),
                                textAlign: TextAlign.center,
                              ),
                              const SizedBox(height: 4),
                              // Breed Traits
                              Text(
                                _getBreedTraits(breed),
                                style: Theme.of(context)
                                    .textTheme
                                    .bodySmall
                                    ?.copyWith(
                                      color: Colors.grey.shade600,
                                    ),
                                textAlign: TextAlign.center,
                                maxLines: 2,
                                overflow: TextOverflow.ellipsis,
                              ),
                            ],
                          ),
                        ),
                      ),
                    );
                  },
                ),
              ),

              const SizedBox(height: 16),

              // Name Input (for onboarding)
              if (widget.isOnboarding && _selectedBreed != null) ...[
                TextField(
                  controller: _nameController,
                  decoration: InputDecoration(
                    labelText: 'Name your guard dog',
                    hintText: 'e.g., Max, Rex, Bella',
                    border: const OutlineInputBorder(),
                    prefixIcon: const Icon(Icons.pets),
                  ),
                  textCapitalization: TextCapitalization.words,
                  maxLength: 20,
                ),
                const SizedBox(height: 16),
              ],

              // Confirm Button
              if (_selectedBreed != null)
                ElevatedButton(
                  onPressed: _confirmSelection,
                  style: ElevatedButton.styleFrom(
                    padding: const EdgeInsets.symmetric(vertical: 16),
                    backgroundColor: Theme.of(context).colorScheme.primary,
                    foregroundColor: Colors.white,
                  ),
                  child: Row(
                    mainAxisAlignment: MainAxisAlignment.center,
                    children: [
                      const Icon(Icons.check_circle),
                      const SizedBox(width: 8),
                      Text(
                        widget.isOnboarding
                            ? 'Start Protecting'
                            : 'Confirm Change',
                        style: const TextStyle(
                          fontSize: 16,
                          fontWeight: FontWeight.bold,
                        ),
                      ),
                    ],
                  ),
                )
              else
                Container(
                  padding: const EdgeInsets.all(16),
                  decoration: BoxDecoration(
                    color: Colors.blue.shade50,
                    borderRadius: BorderRadius.circular(8),
                  ),
                  child: Row(
                    children: [
                      Icon(Icons.info_outline, color: Colors.blue.shade700),
                      const SizedBox(width: 12),
                      Expanded(
                        child: Text(
                          'Select a breed to continue',
                          style: TextStyle(color: Colors.blue.shade700),
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

  String _getBreedTraits(DogBreed breed) {
    switch (breed) {
      case DogBreed.germanShepherd:
        return 'Loyal • Intelligent';
      case DogBreed.rottweiler:
        return 'Powerful • Protective';
      case DogBreed.doberman:
        return 'Alert • Fearless';
      case DogBreed.bulldog:
        return 'Determined • Brave';
      case DogBreed.pitbull:
        return 'Strong • Devoted';
      case DogBreed.husky:
        return 'Energetic • Vocal';
      case DogBreed.beagle:
        return 'Persistent • Clever';
    }
  }

  void _confirmSelection() async {
    if (_selectedBreed == null) return;

    String dogName = _nameController.text.trim();
    if (dogName.isEmpty && widget.isOnboarding) {
      // Use default name based on breed
      dogName = _getDefaultName(_selectedBreed!);
    } else if (dogName.isEmpty) {
      dogName = 'Guard Dog';
    }

    // Create new dog
    final now = DateTime.now();
    final newDog = Dog(
      id: DateTime.now().millisecondsSinceEpoch.toString(),
      name: dogName,
      breed: _selectedBreed!,
      stats: const DogStats(
        hunger: 80,
        happiness: 80,
        energy: 80,
        loyalty: 50,
      ),
      personality: _getPersonalityForBreed(_selectedBreed!),
      createdAt: now,
      lastInteraction: now,
    );

    // Save dog using provider
    await ref.read(dogProvider.notifier).setDog(newDog);

    if (!mounted) return;

    if (widget.isOnboarding) {
      // Navigate to home screen and remove all previous routes
      Navigator.of(context).pushNamedAndRemoveUntil('/', (route) => false);
    } else {
      // Just go back
      Navigator.of(context).pop();
      ScaffoldMessenger.of(context).showSnackBar(
        SnackBar(
          content:
              Text('Your guard dog is now a ${_selectedBreed!.displayName}!'),
          backgroundColor: Colors.green,
        ),
      );
    }
  }

  String _getDefaultName(DogBreed breed) {
    switch (breed) {
      case DogBreed.germanShepherd:
        return 'Rex';
      case DogBreed.rottweiler:
        return 'Tank';
      case DogBreed.doberman:
        return 'Ace';
      case DogBreed.bulldog:
        return 'Bruno';
      case DogBreed.pitbull:
        return 'Max';
      case DogBreed.husky:
        return 'Storm';
      case DogBreed.beagle:
        return 'Scout';
    }
  }

  DogPersonality _getPersonalityForBreed(DogBreed breed) {
    switch (breed) {
      case DogBreed.germanShepherd:
        return const DogPersonality(
          protective: true,
          brave: true,
          playful: false,
          lazy: false,
        );
      case DogBreed.rottweiler:
        return const DogPersonality(
          protective: true,
          brave: true,
          playful: false,
          lazy: false,
        );
      case DogBreed.doberman:
        return const DogPersonality(
          protective: true,
          brave: true,
          playful: false,
          lazy: false,
        );
      case DogBreed.bulldog:
        return const DogPersonality(
          protective: true,
          brave: true,
          playful: false,
          lazy: true,
        );
      case DogBreed.pitbull:
        return const DogPersonality(
          protective: true,
          brave: true,
          playful: true,
          lazy: false,
        );
      case DogBreed.husky:
        return const DogPersonality(
          protective: false,
          brave: false,
          playful: true,
          lazy: false,
        );
      case DogBreed.beagle:
        return const DogPersonality(
          protective: false,
          brave: false,
          playful: true,
          lazy: false,
        );
    }
  }
}
