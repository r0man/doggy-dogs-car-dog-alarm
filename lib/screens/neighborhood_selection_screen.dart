import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import '../models/neighborhood.dart';
import '../providers/neighborhood_provider.dart';
import '../providers/dialogue_provider.dart';
import '../models/dialogue_context.dart';
import '../theme/urban_colors.dart';

/// Screen for selecting current neighborhood/district
class NeighborhoodSelectionScreen extends ConsumerWidget {
  const NeighborhoodSelectionScreen({super.key});

  @override
  Widget build(BuildContext context, WidgetRef ref) {
    final currentState = ref.watch(neighborhoodProvider);
    final mostFrequentAsync = ref.watch(mostFrequentNeighborhoodProvider);

    return Scaffold(
      appBar: AppBar(
        title: const Text('Select Neighborhood'),
        elevation: 0,
      ),
      body: SafeArea(
        child: Column(
          children: [
            // Header with current location
            Container(
              padding: const EdgeInsets.all(16),
              color: UrbanColors.concreteGray,
              child: Column(
                crossAxisAlignment: CrossAxisAlignment.stretch,
                children: [
                  Text(
                    'Where are you parked?',
                    style: Theme.of(context).textTheme.titleLarge,
                  ),
                  const SizedBox(height: 8),
                  if (currentState.currentNeighborhood != null) ...[
                    Row(
                      children: [
                        Text(
                          '📍 Currently in: ',
                          style: Theme.of(context).textTheme.bodyMedium,
                        ),
                        Text(
                          currentState.currentNeighborhood!.displayName,
                          style: Theme.of(context).textTheme.bodyMedium?.copyWith(
                                color: Color(currentState.currentNeighborhood!.style.primaryColor),
                                fontWeight: FontWeight.bold,
                              ),
                        ),
                      ],
                    ),
                    if (currentState.isUsualSpot)
                      Text(
                        '⭐ Your usual spot!',
                        style: Theme.of(context).textTheme.bodySmall?.copyWith(
                              color: UrbanColors.neonYellow,
                            ),
                      ),
                  ],
                  const SizedBox(height: 8),
                  Container(
                    padding: const EdgeInsets.symmetric(horizontal: 12, vertical: 6),
                    decoration: BoxDecoration(
                      color: _getThreatColor(currentState.threatLevel),
                      borderRadius: BorderRadius.circular(4),
                      border: Border.all(color: UrbanColors.comicBlack, width: 2),
                    ),
                    child: Text(
                      'Threat Level: ${currentState.threatLevel}/10 (${currentState.timeOfDay.displayName})',
                      style: Theme.of(context).textTheme.bodySmall?.copyWith(
                            color: UrbanColors.comicWhite,
                            fontWeight: FontWeight.bold,
                          ),
                      textAlign: TextAlign.center,
                    ),
                  ),
                ],
              ),
            ),

            // Most frequent neighborhood hint
            if (mostFrequentAsync.hasValue && mostFrequentAsync.value != null)
              Container(
                padding: const EdgeInsets.all(12),
                margin: const EdgeInsets.all(16),
                decoration: BoxDecoration(
                  color: UrbanColors.neonCyan.withOpacity(0.1),
                  border: Border.all(color: UrbanColors.neonCyan, width: 2),
                  borderRadius: BorderRadius.circular(4),
                ),
                child: Row(
                  children: [
                    const Icon(Icons.stars, color: UrbanColors.neonCyan),
                    const SizedBox(width: 8),
                    Expanded(
                      child: Text(
                        'You usually park in ${mostFrequentAsync.value!.displayName}',
                        style: Theme.of(context).textTheme.bodySmall,
                      ),
                    ),
                  ],
                ),
              ),

            // Neighborhood list
            Expanded(
              child: ListView.builder(
                padding: const EdgeInsets.all(16),
                itemCount: Neighborhood.values.length,
                itemBuilder: (context, index) {
                  final neighborhood = Neighborhood.values[index];
                  final style = neighborhood.style;
                  final isSelected = currentState.currentNeighborhood == neighborhood;

                  return _NeighborhoodTile(
                    neighborhood: neighborhood,
                    style: style,
                    isSelected: isSelected,
                    onTap: () async {
                      await ref.read(neighborhoodProvider.notifier).setNeighborhood(neighborhood);

                      // Trigger parking location dialogue
                      ref.read(dialogueProvider.notifier).showDialogue(
                            DialogueData(
                              context: DialogueContext.parkingLocation,
                              locationDescription: neighborhood.description,
                            ),
                          );

                      if (context.mounted) {
                        Navigator.pop(context);
                      }
                    },
                  );
                },
              ),
            ),
          ],
        ),
      ),
    );
  }

  Color _getThreatColor(int threatLevel) {
    if (threatLevel <= 3) return UrbanColors.successGreen;
    if (threatLevel <= 6) return UrbanColors.warningOrange;
    return UrbanColors.dangerRed;
  }
}

/// Individual neighborhood tile
class _NeighborhoodTile extends ConsumerWidget {
  final Neighborhood neighborhood;
  final NeighborhoodStyle style;
  final bool isSelected;
  final VoidCallback onTap;

  const _NeighborhoodTile({
    required this.neighborhood,
    required this.style,
    required this.isSelected,
    required this.onTap,
  });

  @override
  Widget build(BuildContext context, WidgetRef ref) {
    final statsAsync = ref.watch(neighborhoodStatsProvider(neighborhood));

    return Card(
      margin: const EdgeInsets.only(bottom: 12),
      color: isSelected ? Color(style.primaryColor).withOpacity(0.2) : null,
      child: InkWell(
        onTap: onTap,
        borderRadius: BorderRadius.circular(4),
        child: Padding(
          padding: const EdgeInsets.all(16),
          child: Column(
            crossAxisAlignment: CrossAxisAlignment.start,
            children: [
              Row(
                children: [
                  // Icon
                  Text(
                    style.icon,
                    style: const TextStyle(fontSize: 32),
                  ),
                  const SizedBox(width: 12),

                  // Name and description
                  Expanded(
                    child: Column(
                      crossAxisAlignment: CrossAxisAlignment.start,
                      children: [
                        Row(
                          children: [
                            Text(
                              neighborhood.displayName,
                              style: Theme.of(context).textTheme.titleMedium?.copyWith(
                                    color: Color(style.primaryColor),
                                    fontWeight: FontWeight.bold,
                                  ),
                            ),
                            if (isSelected) ...[
                              const SizedBox(width: 8),
                              Container(
                                padding: const EdgeInsets.symmetric(
                                  horizontal: 8,
                                  vertical: 2,
                                ),
                                decoration: BoxDecoration(
                                  color: UrbanColors.neonCyan,
                                  borderRadius: BorderRadius.circular(4),
                                  border: Border.all(
                                    color: UrbanColors.comicBlack,
                                    width: 2,
                                  ),
                                ),
                                child: Text(
                                  'HERE',
                                  style: Theme.of(context).textTheme.labelSmall?.copyWith(
                                        color: UrbanColors.comicBlack,
                                        fontWeight: FontWeight.bold,
                                      ),
                                ),
                              ),
                            ],
                          ],
                        ),
                        const SizedBox(height: 4),
                        Text(
                          neighborhood.description,
                          style: Theme.of(context).textTheme.bodySmall,
                        ),
                        const SizedBox(height: 4),
                        Text(
                          '${style.atmosphere} • ${neighborhood.security.description}',
                          style: Theme.of(context).textTheme.bodySmall?.copyWith(
                                color: UrbanColors.fog,
                                fontStyle: FontStyle.italic,
                              ),
                        ),
                      ],
                    ),
                  ),
                ],
              ),

              // Stats (if visited before)
              if (statsAsync.hasValue && statsAsync.value!.visitCount > 0) ...[
                const SizedBox(height: 12),
                Container(
                  padding: const EdgeInsets.all(8),
                  decoration: BoxDecoration(
                    color: UrbanColors.asphalt.withOpacity(0.3),
                    borderRadius: BorderRadius.circular(4),
                  ),
                  child: Row(
                    children: [
                      Icon(
                        Icons.history,
                        size: 16,
                        color: Color(style.accentColor),
                      ),
                      const SizedBox(width: 8),
                      Text(
                        'Visited ${statsAsync.value!.visitCount}x',
                        style: Theme.of(context).textTheme.bodySmall,
                      ),
                      if (statsAsync.value!.isFrequent) ...[
                        const SizedBox(width: 8),
                        const Icon(
                          Icons.star,
                          size: 16,
                          color: UrbanColors.neonYellow,
                        ),
                      ],
                    ],
                  ),
                ),
              ],
            ],
          ),
        ),
      ),
    );
  }
}
