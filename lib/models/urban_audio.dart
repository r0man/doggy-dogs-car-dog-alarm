/// Urban soundscape and audio system models
import 'neighborhood.dart';

/// Audio track types
enum AudioTrackType {
  jazz('Jazz', '🎷'),
  ambientCity('City Ambience', '🏙️'),
  alarmSound('Alarm', '🚨'),
  dogBark('Dog Bark', '🐕');

  final String displayName;
  final String icon;

  const AudioTrackType(this.displayName, this.icon);
}

/// Audio track model
class AudioTrack {
  final String id;
  final String name;
  final AudioTrackType type;
  final String description;
  final Duration duration;
  final String? assetPath; // Path to audio file when implemented

  const AudioTrack({
    required this.id,
    required this.name,
    required this.type,
    required this.description,
    required this.duration,
    this.assetPath,
  });
}

/// Urban soundscape - combines multiple audio layers
class UrbanSoundscape {
  final Neighborhood neighborhood;
  final TimeOfDay timeOfDay;
  final List<AudioTrack> layers;
  final double ambientVolume;
  final double musicVolume;

  const UrbanSoundscape({
    required this.neighborhood,
    required this.timeOfDay,
    required this.layers,
    this.ambientVolume = 0.5,
    this.musicVolume = 0.7,
  });

  /// Get soundscape for neighborhood and time
  static UrbanSoundscape forContext({
    required Neighborhood neighborhood,
    required TimeOfDay timeOfDay,
  }) {
    final layers = <AudioTrack>[];

    // Add ambient city sounds
    layers.add(_getAmbientTrack(neighborhood, timeOfDay));

    // Add jazz track
    layers.add(_getJazzTrack(neighborhood, timeOfDay));

    return UrbanSoundscape(
      neighborhood: neighborhood,
      timeOfDay: timeOfDay,
      layers: layers,
    );
  }

  static AudioTrack _getAmbientTrack(
      Neighborhood neighborhood, TimeOfDay timeOfDay) {
    String description;

    switch (neighborhood) {
      case Neighborhood.downtown:
        description = timeOfDay == TimeOfDay.day
            ? 'Traffic, footsteps, distant sirens'
            : 'Muted traffic, wind, occasional voices';
      case Neighborhood.artsQuarter:
        description = timeOfDay == TimeOfDay.day
            ? 'Music from galleries, café chatter'
            : 'Soft music, distant laughter';
      case Neighborhood.industrialZone:
        description = timeOfDay == TimeOfDay.day
            ? 'Machinery, metal clanging, trucks'
            : 'Wind through warehouses, distant trains';
      case Neighborhood.waterfront:
        description = timeOfDay == TimeOfDay.day
            ? 'Seagulls, waves, ship horns'
            : 'Waves, buoy bells, foghorns';
      case Neighborhood.uptown:
        description = timeOfDay == TimeOfDay.day
            ? 'Quiet streets, car doors, heels clicking'
            : 'Very quiet, occasional luxury car';
      case Neighborhood.outskirts:
        description = timeOfDay == TimeOfDay.day
            ? 'Shopping carts, parking lot sounds'
            : 'Crickets, distant highway';
    }

    return AudioTrack(
      id: 'ambient_${neighborhood.name}_${timeOfDay.name}',
      name: '${neighborhood.displayName} ${timeOfDay.displayName}',
      type: AudioTrackType.ambientCity,
      description: description,
      duration: const Duration(minutes: 3),
    );
  }

  static AudioTrack _getJazzTrack(
      Neighborhood neighborhood, TimeOfDay timeOfDay) {
    String name;
    String description;

    switch (neighborhood) {
      case Neighborhood.downtown:
        name = 'Corporate Jazz';
        description = 'Smooth jazz with modern edge';
      case Neighborhood.artsQuarter:
        name = 'Avant-Garde Jazz';
        description = 'Experimental, free-form jazz';
      case Neighborhood.industrialZone:
        name = 'Hard Bop';
        description = 'Gritty, blues-influenced jazz';
      case Neighborhood.waterfront:
        name = 'Maritime Blues';
        description = 'Bluesy jazz with sea shanty vibes';
      case Neighborhood.uptown:
        name = 'Sophisticated Swing';
        description = 'Elegant big band jazz';
      case Neighborhood.outskirts:
        name = 'Roadside Jazz';
        description = 'Easy-going jazz for the road';
    }

    return AudioTrack(
      id: 'jazz_${neighborhood.name}',
      name: name,
      type: AudioTrackType.jazz,
      description: description,
      duration: const Duration(minutes: 4),
    );
  }
}

/// Audio player state
class AudioPlayerState {
  final bool isPlaying;
  final AudioTrack? currentTrack;
  final Duration position;
  final double volume;
  final bool isMuted;

  const AudioPlayerState({
    this.isPlaying = false,
    this.currentTrack,
    this.position = Duration.zero,
    this.volume = 0.7,
    this.isMuted = false,
  });

  AudioPlayerState copyWith({
    bool? isPlaying,
    AudioTrack? currentTrack,
    Duration? position,
    double? volume,
    bool? isMuted,
  }) {
    return AudioPlayerState(
      isPlaying: isPlaying ?? this.isPlaying,
      currentTrack: currentTrack ?? this.currentTrack,
      position: position ?? this.position,
      volume: volume ?? this.volume,
      isMuted: isMuted ?? this.isMuted,
    );
  }
}

/// Jazz album collection
class JazzAlbums {
  static const List<AudioTrack> allTracks = [
    AudioTrack(
      id: 'smooth_streets',
      name: 'Smooth Streets',
      type: AudioTrackType.jazz,
      description: 'Laid-back jazz for easy cruising',
      duration: Duration(minutes: 4, seconds: 30),
    ),
    AudioTrack(
      id: 'urban_nights',
      name: 'Urban Nights',
      type: AudioTrackType.jazz,
      description: 'Moody jazz for nighttime patrol',
      duration: Duration(minutes: 5, seconds: 15),
    ),
    AudioTrack(
      id: 'bebop_bounce',
      name: 'Bebop Bounce',
      type: AudioTrackType.jazz,
      description: 'Upbeat jazz with plenty of energy',
      duration: Duration(minutes: 3, seconds: 45),
    ),
    AudioTrack(
      id: 'concrete_serenade',
      name: 'Concrete Serenade',
      type: AudioTrackType.jazz,
      description: 'Romantic jazz for the city',
      duration: Duration(minutes: 6, seconds: 0),
    ),
    AudioTrack(
      id: 'free_form_frenzy',
      name: 'Free Form Frenzy',
      type: AudioTrackType.jazz,
      description: 'Experimental jazz that pushes boundaries',
      duration: Duration(minutes: 7, seconds: 20),
    ),
  ];
}
