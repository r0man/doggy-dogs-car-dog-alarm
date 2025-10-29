import 'dart:convert';
import 'package:shared_preferences/shared_preferences.dart';
import '../models/neighborhood.dart';

/// Service for managing neighborhood/location awareness
/// Tracks visits, learns patterns, provides context
class NeighborhoodService {
  static const String _visitsKey = 'neighborhood_visits';
  static const String _currentKey = 'current_neighborhood';

  final SharedPreferences _prefs;

  NeighborhoodService(this._prefs);

  /// Get current neighborhood (last set location)
  Neighborhood? getCurrentNeighborhood() {
    final neighborhoodName = _prefs.getString(_currentKey);
    if (neighborhoodName == null) return null;

    try {
      return Neighborhood.values.firstWhere(
        (e) => e.name == neighborhoodName,
      );
    } catch (e) {
      return null;
    }
  }

  /// Set current neighborhood (when user parks)
  Future<void> setCurrentNeighborhood(Neighborhood neighborhood) async {
    await _prefs.setString(_currentKey, neighborhood.name);
    await _recordVisit(neighborhood);
  }

  /// Record a visit to a neighborhood
  Future<void> _recordVisit(Neighborhood neighborhood) async {
    final visits = await getVisitHistory();

    final newVisit = LocationVisit(
      neighborhood: neighborhood,
      timestamp: DateTime.now(),
      duration: const Duration(hours: 1), // Will be updated on departure
    );

    visits.add(newVisit);

    // Keep last 100 visits
    if (visits.length > 100) {
      visits.removeAt(0);
    }

    await _saveVisits(visits);
  }

  /// Update duration of current visit (when user returns)
  Future<void> updateCurrentVisitDuration(Duration duration) async {
    final visits = await getVisitHistory();
    if (visits.isEmpty) return;

    final lastVisit = visits.last;
    final updatedVisit = LocationVisit(
      neighborhood: lastVisit.neighborhood,
      timestamp: lastVisit.timestamp,
      duration: duration,
    );

    visits[visits.length - 1] = updatedVisit;
    await _saveVisits(visits);
  }

  /// Get visit history
  Future<List<LocationVisit>> getVisitHistory() async {
    final visitsJson = _prefs.getString(_visitsKey);
    if (visitsJson == null) return [];

    try {
      final List<dynamic> visitsList = jsonDecode(visitsJson);
      return visitsList
          .map((v) => LocationVisit.fromJson(v as Map<String, dynamic>))
          .toList();
    } catch (e) {
      return [];
    }
  }

  /// Save visits to storage
  Future<void> _saveVisits(List<LocationVisit> visits) async {
    final visitsJson = jsonEncode(visits.map((v) => v.toJson()).toList());
    await _prefs.setString(_visitsKey, visitsJson);
  }

  /// Get most frequent neighborhood (pattern learning)
  Future<Neighborhood?> getMostFrequentNeighborhood() async {
    final visits = await getVisitHistory();
    if (visits.isEmpty) return null;

    // Count visits by neighborhood
    final Map<Neighborhood, int> counts = {};
    for (final visit in visits) {
      counts[visit.neighborhood] = (counts[visit.neighborhood] ?? 0) + 1;
    }

    // Find most frequent
    Neighborhood? mostFrequent;
    int maxCount = 0;
    counts.forEach((neighborhood, count) {
      if (count > maxCount) {
        maxCount = count;
        mostFrequent = neighborhood;
      }
    });

    return mostFrequent;
  }

  /// Check if current neighborhood is a usual spot
  Future<bool> isUsualSpot(Neighborhood neighborhood) async {
    final visits = await getVisitHistory();
    if (visits.length < 5) {
      return false; // Need at least 5 visits to establish pattern
    }

    final neighborhoodVisits =
        visits.where((v) => v.neighborhood == neighborhood).length;

    // Consider it "usual" if visited 3+ times and represents at least 20% of visits
    return neighborhoodVisits >= 3 &&
        (neighborhoodVisits / visits.length) >= 0.2;
  }

  /// Get stats for a neighborhood
  Future<NeighborhoodStats> getNeighborhoodStats(
      Neighborhood neighborhood) async {
    final visits = await getVisitHistory();

    final neighborhoodVisits =
        visits.where((v) => v.neighborhood == neighborhood).toList();

    if (neighborhoodVisits.isEmpty) {
      return NeighborhoodStats(
        neighborhood: neighborhood,
        visitCount: 0,
        totalDuration: Duration.zero,
        lastVisit: null,
      );
    }

    final totalDuration = neighborhoodVisits.fold<Duration>(
        Duration.zero, (sum, visit) => sum + visit.duration);

    final lastVisit = neighborhoodVisits
        .reduce((a, b) => a.timestamp.isAfter(b.timestamp) ? a : b);

    return NeighborhoodStats(
      neighborhood: neighborhood,
      visitCount: neighborhoodVisits.length,
      totalDuration: totalDuration,
      lastVisit: lastVisit.timestamp,
    );
  }

  /// Get commentary for current neighborhood
  String getNeighborhoodCommentary(Neighborhood neighborhood,
      {bool isUsual = false}) {
    final commentaries = neighborhood.commentaries;

    if (isUsual) {
      // Add "usual spot" context
      return '${commentaries[0]} Our usual spot, right?';
    }

    // Return random commentary
    return commentaries[DateTime.now().millisecond % commentaries.length];
  }

  /// Get current threat level based on neighborhood and time
  int getCurrentThreatLevel() {
    final neighborhood = getCurrentNeighborhood();
    if (neighborhood == null) return 5; // Default moderate threat

    final timeOfDay = TimeOfDayExtension.fromDateTime(DateTime.now());
    return neighborhood.security.getThreatLevel(timeOfDay);
  }

  /// Clear all neighborhood data (for testing/reset)
  Future<void> clearAllData() async {
    await _prefs.remove(_visitsKey);
    await _prefs.remove(_currentKey);
  }
}

/// Statistics for a specific neighborhood
class NeighborhoodStats {
  final Neighborhood neighborhood;
  final int visitCount;
  final Duration totalDuration;
  final DateTime? lastVisit;

  const NeighborhoodStats({
    required this.neighborhood,
    required this.visitCount,
    required this.totalDuration,
    this.lastVisit,
  });

  /// Average duration per visit
  Duration get averageDuration {
    if (visitCount == 0) return Duration.zero;
    return Duration(
      seconds: totalDuration.inSeconds ~/ visitCount,
    );
  }

  /// Is this a frequent spot?
  bool get isFrequent => visitCount >= 5;
}
