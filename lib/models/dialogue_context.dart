/// Context for dialogue generation
/// Used to determine appropriate witty responses
enum DialogueContext {
  /// User just activated the alarm
  alarmActivated,

  /// User is checking in on the dog
  checkIn,

  /// Dog detected potential threat
  threatDetected,

  /// False alarm occurred
  falseAlarm,

  /// User returned to car
  userReturned,

  /// Feeding the dog
  feeding,

  /// Playing with the dog
  playing,

  /// Dog is neglected (low stats)
  neglected,

  /// Alarm successfully scared someone off
  alarmSuccess,

  /// User has been gone too long
  goneTooLong,

  /// Parking location commentary
  parkingLocation,

  /// Morning check-in
  morning,

  /// Evening check-in
  evening,

  /// Level up
  levelUp,

  /// Achievement unlocked
  achievement,
}

extension DialogueContextExtension on DialogueContext {
  String get displayName {
    switch (this) {
      case DialogueContext.alarmActivated:
        return 'Alarm Activated';
      case DialogueContext.checkIn:
        return 'Check In';
      case DialogueContext.threatDetected:
        return 'Threat Detected';
      case DialogueContext.falseAlarm:
        return 'False Alarm';
      case DialogueContext.userReturned:
        return 'User Returned';
      case DialogueContext.feeding:
        return 'Feeding';
      case DialogueContext.playing:
        return 'Playing';
      case DialogueContext.neglected:
        return 'Neglected';
      case DialogueContext.alarmSuccess:
        return 'Alarm Success';
      case DialogueContext.goneTooLong:
        return 'Gone Too Long';
      case DialogueContext.parkingLocation:
        return 'Parking Location';
      case DialogueContext.morning:
        return 'Morning';
      case DialogueContext.evening:
        return 'Evening';
      case DialogueContext.levelUp:
        return 'Level Up';
      case DialogueContext.achievement:
        return 'Achievement';
    }
  }
}

/// Additional context data for dialogue generation
class DialogueData {
  final DialogueContext context;
  final Duration? timeGone;
  final String? locationDescription;
  final int? threatLevel;
  final String? achievementName;
  final int? newLevel;

  const DialogueData({
    required this.context,
    this.timeGone,
    this.locationDescription,
    this.threatLevel,
    this.achievementName,
    this.newLevel,
  });
}
