import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import '../models/urban_audio.dart';
import '../models/neighborhood.dart';
import '../theme/urban_colors.dart';
import '../theme/comic_decorations.dart';
import '../providers/neighborhood_provider.dart';

/// Audio player screen for jazz and urban soundscapes
class AudioPlayerScreen extends ConsumerStatefulWidget {
  const AudioPlayerScreen({super.key});

  @override
  ConsumerState<AudioPlayerScreen> createState() => _AudioPlayerScreenState();
}

class _AudioPlayerScreenState extends ConsumerState<AudioPlayerScreen> {
  AudioPlayerState _playerState = const AudioPlayerState();
  int _selectedTrackIndex = 0;

  void _playTrack(AudioTrack track) {
    setState(() {
      _playerState = _playerState.copyWith(
        isPlaying: true,
        currentTrack: track,
        position: Duration.zero,
      );
    });

    // Simulate playback (in real implementation, this would use audio player)
    ScaffoldMessenger.of(context).showSnackBar(
      SnackBar(
        content: Text('Now playing: ${track.name}'),
        backgroundColor: UrbanColors.neonCyan,
        behavior: SnackBarBehavior.floating,
      ),
    );
  }

  void _pauseTrack() {
    setState(() {
      _playerState = _playerState.copyWith(isPlaying: false);
    });
  }

  void _resumeTrack() {
    setState(() {
      _playerState = _playerState.copyWith(isPlaying: true);
    });
  }

  void _stopTrack() {
    setState(() {
      _playerState = const AudioPlayerState();
    });
  }

  @override
  Widget build(BuildContext context) {
    final neighborhoodState = ref.watch(neighborhoodProvider);
    final soundscape = neighborhoodState.currentNeighborhood != null
        ? UrbanSoundscape.forContext(
            neighborhood: neighborhoodState.currentNeighborhood!,
            timeOfDay: neighborhoodState.timeOfDay,
          )
        : null;

    return Scaffold(
      appBar: AppBar(
        title: const Text('Urban Soundscape'),
        elevation: 0,
      ),
      body: SafeArea(
        child: Column(
          children: [
            // Now Playing section
            if (_playerState.currentTrack != null) ...[
              _buildNowPlaying(),
              const Divider(height: 1),
            ],

            // Soundscape section
            if (soundscape != null) ...[
              _buildSoundscapeSection(soundscape),
              const Divider(height: 1),
            ],

            // Jazz tracks section
            Expanded(
              child: _buildJazzTracksSection(),
            ),
          ],
        ),
      ),
    );
  }

  Widget _buildNowPlaying() {
    final track = _playerState.currentTrack!;

    return Container(
      padding: const EdgeInsets.all(16),
      decoration: BoxDecoration(
        gradient: LinearGradient(
          colors: [
            UrbanColors.concreteGray,
            UrbanColors.asphalt.withOpacity(0.5),
          ],
          begin: Alignment.topLeft,
          end: Alignment.bottomRight,
        ),
      ),
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.stretch,
        children: [
          Row(
            children: [
              Text(
                track.type.icon,
                style: const TextStyle(fontSize: 48),
              ),
              const SizedBox(width: 16),
              Expanded(
                child: Column(
                  crossAxisAlignment: CrossAxisAlignment.start,
                  children: [
                    Text(
                      track.name,
                      style: Theme.of(context).textTheme.titleLarge?.copyWith(
                            color: UrbanColors.neonCyan,
                            fontWeight: FontWeight.bold,
                          ),
                    ),
                    Text(
                      track.description,
                      style: Theme.of(context).textTheme.bodySmall,
                    ),
                  ],
                ),
              ),
            ],
          ),
          const SizedBox(height: 16),
          Row(
            mainAxisAlignment: MainAxisAlignment.center,
            children: [
              IconButton(
                onPressed: _stopTrack,
                icon: const Icon(Icons.stop),
                iconSize: 32,
              ),
              const SizedBox(width: 16),
              Container(
                decoration: const BoxDecoration(
                  shape: BoxShape.circle,
                  color: UrbanColors.neonCyan,
                  boxShadow: [ComicDecorations.dropShadow],
                ),
                child: IconButton(
                  onPressed:
                      _playerState.isPlaying ? _pauseTrack : _resumeTrack,
                  icon: Icon(
                      _playerState.isPlaying ? Icons.pause : Icons.play_arrow),
                  iconSize: 48,
                  color: UrbanColors.comicBlack,
                ),
              ),
              const SizedBox(width: 16),
              IconButton(
                onPressed: () {
                  // Next track
                  final nextIndex =
                      (_selectedTrackIndex + 1) % JazzAlbums.allTracks.length;
                  _playTrack(JazzAlbums.allTracks[nextIndex]);
                  setState(() {
                    _selectedTrackIndex = nextIndex;
                  });
                },
                icon: const Icon(Icons.skip_next),
                iconSize: 32,
              ),
            ],
          ),
        ],
      ),
    );
  }

  Widget _buildSoundscapeSection(UrbanSoundscape soundscape) {
    return Container(
      padding: const EdgeInsets.all(16),
      color: UrbanColors.concreteGray,
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.stretch,
        children: [
          Text(
            'Neighborhood Soundscape',
            style: Theme.of(context).textTheme.titleMedium?.copyWith(
                  fontWeight: FontWeight.bold,
                ),
          ),
          const SizedBox(height: 8),
          Text(
            '${soundscape.neighborhood.displayName} • ${soundscape.timeOfDay.displayName}',
            style: Theme.of(context).textTheme.bodySmall?.copyWith(
                  color: UrbanColors.fog,
                ),
          ),
          const SizedBox(height: 12),
          Wrap(
            spacing: 8,
            runSpacing: 8,
            children: soundscape.layers.map((track) {
              return Chip(
                avatar: Text(track.type.icon),
                label: Text(track.name),
                backgroundColor: UrbanColors.asphalt.withOpacity(0.3),
                side: const BorderSide(color: UrbanColors.comicBlack, width: 2),
              );
            }).toList(),
          ),
          const SizedBox(height: 12),
          ElevatedButton.icon(
            onPressed: () {
              _playTrack(soundscape.layers.first);
            },
            icon: const Icon(Icons.play_arrow),
            label: const Text('Play Soundscape'),
            style: ElevatedButton.styleFrom(
              backgroundColor: UrbanColors.warningOrange,
              foregroundColor: UrbanColors.comicBlack,
            ),
          ),
        ],
      ),
    );
  }

  Widget _buildJazzTracksSection() {
    return ListView(
      padding: const EdgeInsets.all(16),
      children: [
        Text(
          'Jazz Collection',
          style: Theme.of(context).textTheme.titleLarge?.copyWith(
                fontWeight: FontWeight.bold,
              ),
        ),
        const SizedBox(height: 16),
        ...JazzAlbums.allTracks.asMap().entries.map((entry) {
          final index = entry.key;
          final track = entry.value;
          final isPlaying = _playerState.currentTrack?.id == track.id &&
              _playerState.isPlaying;

          return Card(
            margin: const EdgeInsets.only(bottom: 12),
            color: isPlaying ? UrbanColors.neonCyan.withOpacity(0.2) : null,
            child: ListTile(
              leading: Container(
                width: 48,
                height: 48,
                decoration: BoxDecoration(
                  color: isPlaying ? UrbanColors.neonCyan : UrbanColors.asphalt,
                  borderRadius: BorderRadius.circular(8),
                  border: Border.all(color: UrbanColors.comicBlack, width: 2),
                ),
                child: Center(
                  child: Text(
                    track.type.icon,
                    style: const TextStyle(fontSize: 24),
                  ),
                ),
              ),
              title: Text(
                track.name,
                style: const TextStyle(fontWeight: FontWeight.bold),
              ),
              subtitle: Column(
                crossAxisAlignment: CrossAxisAlignment.start,
                children: [
                  Text(track.description),
                  const SizedBox(height: 4),
                  Text(
                    _formatDuration(track.duration),
                    style: Theme.of(context).textTheme.bodySmall?.copyWith(
                          color: UrbanColors.fog,
                        ),
                  ),
                ],
              ),
              trailing: IconButton(
                icon: Icon(
                  isPlaying ? Icons.pause_circle : Icons.play_circle,
                  size: 40,
                  color:
                      isPlaying ? UrbanColors.neonCyan : UrbanColors.neonYellow,
                ),
                onPressed: () {
                  if (isPlaying) {
                    _pauseTrack();
                  } else {
                    _playTrack(track);
                    setState(() {
                      _selectedTrackIndex = index;
                    });
                  }
                },
              ),
              isThreeLine: true,
            ),
          );
        }),
      ],
    );
  }

  String _formatDuration(Duration duration) {
    final minutes = duration.inMinutes;
    final seconds = duration.inSeconds.remainder(60);
    return '$minutes:${seconds.toString().padLeft(2, '0')}';
  }
}
