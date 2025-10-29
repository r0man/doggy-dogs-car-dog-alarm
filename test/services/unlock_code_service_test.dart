import 'package:flutter_test/flutter_test.dart';
import 'package:shared_preferences/shared_preferences.dart';
import 'package:doggy_dogs_car_alarm/services/unlock_code_service.dart';

void main() {
  group('UnlockCodeService', () {
    late SharedPreferences prefs;
    late UnlockCodeService service;

    setUp(() async {
      SharedPreferences.setMockInitialValues({});
      prefs = await SharedPreferences.getInstance();
      service = UnlockCodeService(prefs);
    });

    tearDown(() async {
      await prefs.clear();
    });

    test('hasUnlockCode returns false initially', () async {
      final hasCode = await service.hasUnlockCode();
      expect(hasCode, isFalse);
    });

    test('setUnlockCode stores hashed code', () async {
      await service.setUnlockCode('1234');

      final hasCode = await service.hasUnlockCode();
      expect(hasCode, isTrue);

      // Verify it's not stored in plaintext
      final storedValue = prefs.getString('unlock_code_hash');
      expect(storedValue, isNotNull);
      expect(storedValue, isNot('1234'));
    });

    test('setUnlockCode throws on empty code', () async {
      expect(
        () => service.setUnlockCode(''),
        throwsArgumentError,
      );
    });

    test('validateUnlockCode returns true for correct code', () async {
      await service.setUnlockCode('5678');

      final isValid = await service.validateUnlockCode('5678');
      expect(isValid, isTrue);
    });

    test('validateUnlockCode returns false for incorrect code', () async {
      await service.setUnlockCode('5678');

      final isValid = await service.validateUnlockCode('1234');
      expect(isValid, isFalse);
    });

    test('validateUnlockCode uses default code when none set', () async {
      final isValid = await service.validateUnlockCode('1234');
      expect(isValid, isTrue);
    });

    test('validateUnlockCode sets default code on first use', () async {
      final hasCodeBefore = await service.hasUnlockCode();
      expect(hasCodeBefore, isFalse);

      await service.validateUnlockCode('1234');

      final hasCodeAfter = await service.hasUnlockCode();
      expect(hasCodeAfter, isTrue);
    });

    test('resetToDefault sets code to default', () async {
      await service.setUnlockCode('9999');
      await service.resetToDefault();

      final isValid = await service.validateUnlockCode('1234');
      expect(isValid, isTrue);
    });

    test('clearUnlockCode removes stored code', () async {
      await service.setUnlockCode('1234');
      await service.clearUnlockCode();

      final hasCode = await service.hasUnlockCode();
      expect(hasCode, isFalse);
    });

    test('getDefaultCode returns expected default', () {
      expect(service.getDefaultCode(), '1234');
    });

    test('different codes produce different hashes', () async {
      await service.setUnlockCode('1111');
      final hash1 = prefs.getString('unlock_code_hash');

      await service.setUnlockCode('2222');
      final hash2 = prefs.getString('unlock_code_hash');

      expect(hash1, isNot(hash2));
    });

    test('same code produces same hash', () async {
      await service.setUnlockCode('1234');
      final hash1 = prefs.getString('unlock_code_hash');

      await service.setUnlockCode('1234');
      final hash2 = prefs.getString('unlock_code_hash');

      expect(hash1, hash2);
    });

    test('validates complex codes', () async {
      const complexCode = '123456';
      await service.setUnlockCode(complexCode);

      final isValid = await service.validateUnlockCode(complexCode);
      expect(isValid, isTrue);

      final isInvalid = await service.validateUnlockCode('654321');
      expect(isInvalid, isFalse);
    });
  });
}
