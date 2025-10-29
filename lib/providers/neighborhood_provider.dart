import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:shared_preferences/shared_preferences.dart';
import '../models/neighborhood.dart';
import '../services/neighborhood_service.dart';
import '../services/app_settings_service.dart';

/// Provider for neighborhood service
final neighborhoodServiceProvider = Provider<NeighborhoodService>((ref) {
  final prefs = ref.watch(sharedPreferencesProvider);
  return NeighborhoodService(prefs);
});

/// Current neighborhood state
class NeighborhoodState {
  final Neighborhood? currentNeighborhood;
  final bool isUsualSpot;
  final int threatLevel;
  final TimeOfDay timeOfDay;

  const NeighborhoodState({
    this.currentNeighborhood,
    this.isUsualSpot = false,
    this.threatLevel = 5,
    this.timeOfDay = TimeOfDay.day,
  });

  NeighborhoodState copyWith({
    Neighborhood? currentNeighborhood,
    bool? isUsualSpot,
    int? threatLevel,
    TimeOfDay? timeOfDay,
  }) {
    return NeighborhoodState(
      currentNeighborhood: currentNeighborhood ?? this.currentNeighborhood,
      isUsualSpot: isUsualSpot ?? this.isUsualSpot,
      threatLevel: threatLevel ?? this.threatLevel,
      timeOfDay: timeOfDay ?? this.timeOfDay,
    );
  }
}

/// Neighborhood state notifier
class NeighborhoodNotifier extends StateNotifier<NeighborhoodState> {
  NeighborhoodNotifier(this.ref) : super(const NeighborhoodState()) {
    _loadCurrentNeighborhood();
  }

  final Ref ref;

  /// Load current neighborhood from storage
  Future<void> _loadCurrentNeighborhood() async {
    final service = ref.read(neighborhoodServiceProvider);
    final neighborhood = service.getCurrentNeighborhood();

    if (neighborhood != null) {
      await setNeighborhood(neighborhood, updateService: false);
    }

    // Update time of day
    _updateTimeOfDay();
  }

  /// Set current neighborhood
  Future<void> setNeighborhood(
    Neighborhood neighborhood, {
    bool updateService = true,
  }) async {
    final service = ref.read(neighborhoodServiceProvider);

    if (updateService) {
      await service.setCurrentNeighborhood(neighborhood);
    }

    final isUsual = await service.isUsualSpot(neighborhood);
    final threatLevel = service.getCurrentThreatLevel();
    final timeOfDay = TimeOfDayExtension.fromDateTime(DateTime.now());

    state = NeighborhoodState(
      currentNeighborhood: neighborhood,
      isUsualSpot: isUsual,
      threatLevel: threatLevel,
      timeOfDay: timeOfDay,
    );
  }

  /// Update time of day and recalculate threat level
  void _updateTimeOfDay() {
    final timeOfDay = TimeOfDayExtension.fromDateTime(DateTime.now());
    final service = ref.read(neighborhoodServiceProvider);
    final threatLevel = service.getCurrentThreatLevel();

    state = state.copyWith(
      timeOfDay: timeOfDay,
      threatLevel: threatLevel,
    );
  }

  /// Update visit duration (when user returns)
  Future<void> updateVisitDuration(Duration duration) async {
    final service = ref.read(neighborhoodServiceProvider);
    await service.updateCurrentVisitDuration(duration);
  }

  /// Get commentary for current neighborhood
  String? getCommentary() {
    if (state.currentNeighborhood == null) return null;

    final service = ref.read(neighborhoodServiceProvider);
    return service.getNeighborhoodCommentary(
      state.currentNeighborhood!,
      isUsual: state.isUsualSpot,
    );
  }
}

/// Neighborhood state provider
final neighborhoodProvider =
    StateNotifierProvider<NeighborhoodNotifier, NeighborhoodState>((ref) {
  return NeighborhoodNotifier(ref);
});

/// Neighborhood stats provider
final neighborhoodStatsProvider = FutureProvider.family<NeighborhoodStats, Neighborhood>(
  (ref, neighborhood) async {
    final service = ref.watch(neighborhoodServiceProvider);
    return service.getNeighborhoodStats(neighborhood);
  },
);

/// Most frequent neighborhood provider
final mostFrequentNeighborhoodProvider = FutureProvider<Neighborhood?>((ref) async {
  final service = ref.watch(neighborhoodServiceProvider);
  return service.getMostFrequentNeighborhood();
});
