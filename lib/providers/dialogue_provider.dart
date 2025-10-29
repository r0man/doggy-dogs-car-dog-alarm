import 'package:flutter_riverpod/flutter_riverpod.dart';
import '../models/dog.dart';
import '../models/dialogue_context.dart';
import '../services/dialogue_service.dart';
import 'dog_provider.dart';

/// Provider for dialogue service
final dialogueServiceProvider = Provider<DialogueService>((ref) {
  return DialogueService();
});

/// Current dialogue state
class DialogueState {
  final String? currentDialogue;
  final DateTime? timestamp;

  const DialogueState({
    this.currentDialogue,
    this.timestamp,
  });

  DialogueState copyWith({
    String? currentDialogue,
    DateTime? timestamp,
  }) {
    return DialogueState(
      currentDialogue: currentDialogue ?? this.currentDialogue,
      timestamp: timestamp ?? this.timestamp,
    );
  }
}

/// Dialogue state provider
class DialogueNotifier extends StateNotifier<DialogueState> {
  DialogueNotifier(this.ref) : super(const DialogueState());

  final Ref ref;

  /// Show dialogue for specific context
  void showDialogue(DialogueData data) {
    final dog = ref.read(dogProvider);
    if (dog == null) return;

    final dialogueService = ref.read(dialogueServiceProvider);
    final dialogue = dialogueService.getDialogue(
      dog: dog,
      data: data,
    );

    state = DialogueState(
      currentDialogue: dialogue,
      timestamp: DateTime.now(),
    );
  }

  /// Clear current dialogue
  void clearDialogue() {
    state = const DialogueState();
  }

  /// Quick helpers for common contexts
  void onAlarmActivated() {
    showDialogue(const DialogueData(context: DialogueContext.alarmActivated));
  }

  void onCheckIn() {
    showDialogue(const DialogueData(context: DialogueContext.checkIn));
  }

  void onFeeding() {
    showDialogue(const DialogueData(context: DialogueContext.feeding));
  }

  void onPlaying() {
    showDialogue(const DialogueData(context: DialogueContext.playing));
  }

  void onThreatDetected(int threatLevel) {
    showDialogue(DialogueData(
      context: DialogueContext.threatDetected,
      threatLevel: threatLevel,
    ));
  }

  void onFalseAlarm() {
    showDialogue(const DialogueData(context: DialogueContext.falseAlarm));
  }

  void onUserReturned(Duration timeGone) {
    showDialogue(DialogueData(
      context: DialogueContext.userReturned,
      timeGone: timeGone,
    ));
  }

  void onLevelUp(int newLevel) {
    showDialogue(DialogueData(
      context: DialogueContext.levelUp,
      newLevel: newLevel,
    ));
  }
}

final dialogueProvider = StateNotifierProvider<DialogueNotifier, DialogueState>((ref) {
  return DialogueNotifier(ref);
});
