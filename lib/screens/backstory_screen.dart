import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import '../models/dog_backstory.dart';
import '../models/neighborhood.dart';
import '../providers/backstory_provider.dart';
import '../theme/urban_colors.dart';
import '../theme/comic_decorations.dart';

/// Screen displaying dog's backstory
class BackstoryScreen extends ConsumerWidget {
  const BackstoryScreen({super.key});

  @override
  Widget build(BuildContext context, WidgetRef ref) {
    final backstoryAsync = ref.watch(backstoryProvider);

    return Scaffold(
      appBar: AppBar(
        title: const Text('Dog Profile'),
        elevation: 0,
      ),
      body: backstoryAsync.when(
        data: (backstory) {
          if (backstory == null) {
            return const Center(child: Text('No backstory available'));
          }

          return SingleChildScrollView(
            padding: const EdgeInsets.all(16),
            child: Column(
              crossAxisAlignment: CrossAxisAlignment.stretch,
              children: [
                // Header card
                ComicPanelWidget(
                  backgroundColor: UrbanColors.concreteGray,
                  child: Column(
                    crossAxisAlignment: CrossAxisAlignment.start,
                    children: [
                      Text(
                        backstory.name,
                        style:
                            Theme.of(context).textTheme.displaySmall?.copyWith(
                                  color: UrbanColors.rustOrange,
                                ),
                      ),
                      const SizedBox(height: 8),
                      Text(
                        backstory.tagline,
                        style: Theme.of(context).textTheme.titleMedium,
                      ),
                    ],
                  ),
                ),

                const SizedBox(height: 16),

                // Personality archetype
                _buildInfoCard(
                  context,
                  icon: '🎭',
                  title: 'Personality',
                  subtitle: backstory.archetype.displayName,
                  content: backstory.archetype.description,
                ),

                const SizedBox(height: 12),

                // Origin
                _buildInfoCard(
                  context,
                  icon: '📍',
                  title: 'Origin',
                  subtitle: backstory.originNeighborhood.displayName,
                  content: backstory.originNeighborhood.description,
                ),

                const SizedBox(height: 12),

                // Former occupation
                _buildInfoCard(
                  context,
                  icon: '💼',
                  title: 'Background',
                  subtitle: backstory.occupation.displayName,
                  content: backstory.occupation.backstory,
                ),

                const SizedBox(height: 12),

                // Defining moment
                _buildInfoCard(
                  context,
                  icon: '⚡',
                  title: 'Defining Moment',
                  subtitle: null,
                  content: backstory.definingMoment,
                ),

                const SizedBox(height: 12),

                // Personal vendetta
                _buildInfoCard(
                  context,
                  icon: '⚔️',
                  title: 'Personal Vendetta',
                  subtitle: null,
                  content: backstory.personalVendetta,
                ),

                const SizedBox(height: 12),

                // Future dreams
                _buildInfoCard(
                  context,
                  icon: '🌟',
                  title: 'Dreams',
                  subtitle: null,
                  content: backstory.futureDream,
                ),
              ],
            ),
          );
        },
        loading: () => const Center(child: CircularProgressIndicator()),
        error: (err, stack) => Center(child: Text('Error: $err')),
      ),
    );
  }

  Widget _buildInfoCard(
    BuildContext context, {
    required String icon,
    required String title,
    String? subtitle,
    required String content,
  }) {
    return Card(
      child: Padding(
        padding: const EdgeInsets.all(16),
        child: Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            Row(
              children: [
                Text(icon, style: const TextStyle(fontSize: 24)),
                const SizedBox(width: 12),
                Text(
                  title,
                  style: Theme.of(context).textTheme.titleMedium?.copyWith(
                        color: UrbanColors.rustOrange,
                      ),
                ),
              ],
            ),
            if (subtitle != null) ...[
              const SizedBox(height: 8),
              Text(
                subtitle,
                style: Theme.of(context).textTheme.titleSmall,
              ),
            ],
            const SizedBox(height: 8),
            Text(
              content,
              style: Theme.of(context).textTheme.bodyMedium,
            ),
          ],
        ),
      ),
    );
  }
}
