import 'dart:convert';
import 'package:crypto/crypto.dart';
import 'package:shared_preferences/shared_preferences.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'app_settings_service.dart';

/// Service for managing alarm unlock codes with secure storage
class UnlockCodeService {
  static const String _unlockCodeHashKey = 'unlock_code_hash';
  static const String _defaultCode = '1234'; // Default unlock code

  final SharedPreferences _prefs;

  UnlockCodeService(this._prefs);

  /// Hash a code using SHA-256
  String _hashCode(String code) {
    final bytes = utf8.encode(code);
    final digest = sha256.convert(bytes);
    return digest.toString();
  }

  /// Check if an unlock code has been set
  Future<bool> hasUnlockCode() async {
    return _prefs.containsKey(_unlockCodeHashKey);
  }

  /// Set a new unlock code (stores hashed)
  Future<void> setUnlockCode(String code) async {
    if (code.isEmpty) {
      throw ArgumentError('Unlock code cannot be empty');
    }

    final hashedCode = _hashCode(code);
    await _prefs.setString(_unlockCodeHashKey, hashedCode);
  }

  /// Validate an unlock code
  Future<bool> validateUnlockCode(String code) async {
    // If no code set, use default
    if (!await hasUnlockCode()) {
      await setUnlockCode(_defaultCode);
    }

    final storedHash = _prefs.getString(_unlockCodeHashKey);
    if (storedHash == null) return false;

    final inputHash = _hashCode(code);
    return inputHash == storedHash;
  }

  /// Reset to default unlock code
  Future<void> resetToDefault() async {
    await setUnlockCode(_defaultCode);
  }

  /// Clear unlock code (not recommended for security)
  Future<void> clearUnlockCode() async {
    await _prefs.remove(_unlockCodeHashKey);
  }

  /// Get default code (for first-time setup info)
  String getDefaultCode() => _defaultCode;
}

/// Provider for UnlockCodeService
final unlockCodeServiceProvider = Provider<UnlockCodeService>((ref) {
  final prefs = ref.watch(sharedPreferencesProvider);
  return UnlockCodeService(prefs);
});

/// Provider for countdown duration in seconds (from app settings)
final countdownDurationProvider = Provider<int>((ref) {
  final settings = ref.watch(appSettingsProvider);
  return settings.countdownDuration;
});
