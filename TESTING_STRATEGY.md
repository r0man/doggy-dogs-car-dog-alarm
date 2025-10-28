# Testing Strategy: Doggy Dogs Car Dog Alarm

**Version**: 1.0
**Last Updated**: 2025-10-28
**Status**: Active

---

## Testing Philosophy

### Core Principles

1. **Test Everything That Matters**
   - Every feature has acceptance criteria
   - Every acceptance criterion has tests
   - Every test must pass before merge

2. **Automate Relentlessly**
   - Automated tests run on every commit
   - Manual tests only for UX/subjective quality
   - CI/CD blocks merge on test failures

3. **Test Pyramid**
   ```
          /\
         /  \  E2E Tests (10%)
        /____\
       /      \  Integration Tests (30%)
      /________\
     /          \  Unit Tests (60%)
    /____________\
   ```

4. **Quality Gates**
   - Code coverage ≥80% overall
   - Critical paths ≥90% coverage
   - All tests must pass
   - No flaky tests allowed

---

## Test Types

### 1. Unit Tests (60% of tests)

**Purpose**: Test individual functions/classes in isolation

**Coverage Target**: ≥85%

**Examples**:
- Dog model methods (XP calculation, neglect detection, stat decay)
- Sensor threshold calculations
- Audio player state management
- XP/level formulas
- Achievement unlock logic

**Tools**:
- `flutter_test` package
- Mockito for mocking dependencies
- Test data builders

**Run Command**:
```bash
flutter test test/
```

**Requirements**:
- Fast (<1ms per test)
- No external dependencies
- Deterministic (no random failures)
- Clear test names (`test('calculates XP for next level correctly')`)

---

### 2. Widget Tests (25% of tests)

**Purpose**: Test UI components and interactions

**Coverage Target**: ≥75%

**Examples**:
- Home screen displays dog correctly
- Breed selection grid shows all breeds
- Stat bars update when values change
- Buttons trigger correct actions
- Forms validate input

**Tools**:
- `flutter_test` with `WidgetTester`
- `find` matchers
- Gesture simulation

**Run Command**:
```bash
flutter test test/ --tags widget
```

**Requirements**:
- Test in isolation (pump single widget)
- Verify visual output
- Test user interactions
- Check accessibility

---

### 3. Integration Tests (15% of tests)

**Purpose**: Test feature workflows end-to-end

**Coverage Target**: ≥70%

**Examples**:
- Full alarm activation → detection → trigger → deactivation flow
- Daily check-in increments streak and awards XP
- Feeding updates stats and persists to database
- Sensor detection triggers bark sound

**Tools**:
- `integration_test` package
- Real app instances
- Database interactions

**Run Command**:
```bash
flutter test integration_test/
```

**Requirements**:
- Test real flows
- Use real database (test instance)
- Verify state persistence
- Test timing/async operations

---

### 4. Manual Tests (Exploratory)

**Purpose**: Test subjective quality, UX, edge cases

**When**: Before releases, after major features

**Examples**:
- Dog animations look cute and smooth
- App feels responsive
- UX is intuitive
- Real-world car environment testing
- Battery drain testing
- Different Android versions

**Documentation**: Manual test plans in `/docs/testing/manual/`

---

## Test Coverage Requirements

### By Component

| Component | Unit Tests | Widget Tests | Integration Tests | Total Target |
|-----------|-----------|--------------|-------------------|--------------|
| Models (Dog, etc.) | 90% | N/A | 10% | ≥90% |
| Services (Sensor, Audio, etc.) | 85% | N/A | 15% | ≥85% |
| UI Screens | 50% | 80% | 20% | ≥80% |
| Widgets (components) | 40% | 90% | 10% | ≥80% |
| State Management | 80% | 20% | 20% | ≥80% |
| Database/Storage | 80% | N/A | 20% | ≥80% |

### Critical Paths (≥90% Coverage)

These features MUST have ≥90% coverage:

1. **Sensor Detection** - Core functionality, safety-critical
2. **Alarm State Machine** - Must be bulletproof
3. **XP/Level Calculation** - Affects entire progression
4. **Data Persistence** - Can't lose user data
5. **Background Monitoring** - Must survive edge cases

---

## Test Organization

### Directory Structure

```
test/
├── models/                  # Unit tests for models
│   ├── dog_test.dart
│   ├── achievement_test.dart
│   └── ...
├── services/                # Unit tests for services
│   ├── sensor_service_test.dart
│   ├── audio_service_test.dart
│   └── ...
├── screens/                 # Widget tests for screens
│   ├── home_screen_test.dart
│   ├── breed_selection_test.dart
│   └── ...
├── widgets/                 # Widget tests for components
│   ├── dog_widget_test.dart
│   ├── stat_bar_test.dart
│   └── ...
├── integration/             # Integration tests
│   ├── alarm_flow_test.dart
│   ├── daily_checkin_test.dart
│   └── ...
└── helpers/                 # Test utilities
    ├── test_data.dart
    ├── mocks.dart
    └── matchers.dart
```

### Naming Conventions

- Test files: `{feature}_test.dart`
- Test groups: `group('{Feature} Tests', () {...})`
- Test cases: `test('{action} {expected result}', () {...})`

**Examples**:
```dart
// Good
test('calculates XP required for next level correctly')
test('dog is neglected when hunger drops below 30')
test('breed selection persists after app restart')

// Bad
test('test1')
test('it works')
test('xp')
```

---

## Testing Best Practices

### 1. AAA Pattern

All tests follow Arrange-Act-Assert:

```dart
test('dog is neglected when hunger is low', () {
  // Arrange
  final dog = Dog(
    id: '1',
    name: 'Max',
    breed: DogBreed.germanShepherd,
    stats: DogStats(hunger: 20, happiness: 80, energy: 80, loyalty: 50),
    // ...
  );

  // Act
  final isNeglected = dog.isNeglected;

  // Assert
  expect(isNeglected, true);
});
```

### 2. Test Data Builders

Use builders for complex test data:

```dart
// Bad
final dog = Dog(id: '1', name: 'Max', breed: ..., stats: ..., personality: ..., ...);

// Good
final dog = DogTestBuilder()
  .withHunger(20)
  .build();
```

### 3. Descriptive Assertions

```dart
// Bad
expect(dog.effectiveness, 30);

// Good
expect(dog.effectiveness, 30, reason: 'Neglected dogs have fixed 30 effectiveness');
```

### 4. Test Independence

- Each test runs in isolation
- No shared state between tests
- Tests can run in any order
- Use `setUp()` and `tearDown()` for common setup/cleanup

### 5. Mock External Dependencies

```dart
// Mock sensor data
final mockSensorService = MockSensorService();
when(mockSensorService.getAccelerometer()).thenAnswer((_) => Stream.value(0.5));

// Test sensor detection
final detector = MotionDetector(mockSensorService);
await detector.start();
expect(detector.isMotionDetected, false);
```

---

## Continuous Integration (CI)

### GitHub Actions Workflow

Every PR triggers:

1. **Lint** - `flutter analyze`
2. **Format Check** - `flutter format --set-exit-if-changed`
3. **Unit Tests** - `flutter test`
4. **Coverage Report** - Generate and upload to Codecov
5. **Build APK** - Ensure app compiles
6. **Integration Tests** - On emulator

### Branch Protection

Main branch requires:
- ✅ All tests pass
- ✅ Code coverage ≥80%
- ✅ No linting errors
- ✅ Code review approved
- ✅ Commits signed

### Test Performance

- Unit tests: <5 seconds total
- Widget tests: <30 seconds total
- Integration tests: <3 minutes total
- **Total CI time**: <5 minutes

Slow tests block CI and frustrate developers!

---

## Test Maintenance

### When to Write Tests

**Before coding (TDD)**:
- Complex business logic
- Critical features (alarm, sensors)
- Bug fixes (regression tests)

**During coding**:
- Most features

**After coding (Not ideal, but acceptable)**:
- Simple UI components
- Refactoring existing code

### When to Update Tests

- Feature changes → Update related tests
- Bug found → Add regression test
- Refactoring → Tests should still pass (if they don't, they're testing implementation not behavior)

### Flaky Tests

**Zero Tolerance Policy**

- Flaky tests erode trust in test suite
- Fix immediately or disable test
- Root cause: timing issues, race conditions, external dependencies
- Solutions: Better mocks, explicit waits, deterministic data

---

## Code Coverage

### Measuring Coverage

```bash
# Generate coverage report
flutter test --coverage

# View in browser (requires lcov)
genhtml coverage/lcov.info -o coverage/html
open coverage/html/index.html
```

### Coverage Badges

Add to README.md:
```markdown
![Coverage](https://img.shields.io/codecov/c/github/yourusername/doggy-dogs-car-alarm)
```

### What Coverage Doesn't Tell You

- ✅ Lines executed
- ❌ Logic correctness
- ❌ Edge cases tested
- ❌ Test quality

**100% coverage ≠ bug-free code**

But <80% coverage = likely untested bugs!

---

## Testing Tools & Libraries

### Core Testing

- `flutter_test` - Flutter's testing framework
- `mockito` - Mocking dependencies
- `integration_test` - E2E testing

### Additional Tools

- `golden_toolkit` - Screenshot testing
- `patrol` - Advanced integration testing
- `mocktail` - Alternative to Mockito
- `faker` - Generate test data
- `test_coverage` - Better coverage reports

### CI/CD

- GitHub Actions - CI/CD pipeline
- Codecov - Coverage reporting
- Firebase Test Lab - Real device testing

---

## Acceptance Criteria & Testing

Every issue has acceptance criteria with specific test requirements:

**Example** (Sensor Detection - Issue #2):

```
AUTOMATED TESTS REQUIRED:
✓ Unit Tests:
  - SensorService detects motion above threshold
  - SensorService ignores motion below threshold
  - Sensitivity calculation is correct

✓ Widget Tests:
  - Sensitivity slider updates threshold
  - Sensor status displays correctly

✓ Integration Tests:
  - Simulated sensor data triggers detection
  - Detection state propagates to UI

DONE WHEN:
- All automated tests pass
- Code coverage ≥80% for sensor service
```

---

## Definition of Done

A feature is DONE when:

1. ✅ All acceptance criteria met
2. ✅ Automated tests written and passing
3. ✅ Code coverage meets threshold
4. ✅ Manual testing completed (if required)
5. ✅ Code reviewed and approved
6. ✅ Documentation updated
7. ✅ No known bugs
8. ✅ Deployed to test environment

**No shortcuts.** Half-tested features create technical debt.

---

## Testing Metrics

### Track These Metrics

1. **Code Coverage** - Target ≥80%, Critical ≥90%
2. **Test Execution Time** - Keep CI <5 minutes
3. **Flaky Test Rate** - Target 0%, Max 1%
4. **Bug Escape Rate** - Bugs found in production vs. tests
5. **Test Debt** - Features without tests

### Monthly Review

Review testing metrics in team meetings:
- Are we meeting coverage targets?
- Are tests fast enough?
- Any flaky tests?
- What bugs escaped to production?

---

## Resources

### Learning

- [Flutter Testing Guide](https://docs.flutter.dev/testing)
- [Effective Dart: Testing](https://dart.dev/guides/language/effective-dart/testing)
- [Test-Driven Development (Book)](https://www.amazon.com/Test-Driven-Development-Kent-Beck/dp/0321146530)

### Templates

- Test data builders: `/test/helpers/test_data.dart`
- Mock templates: `/test/helpers/mocks.dart`
- Custom matchers: `/test/helpers/matchers.dart`

### Help

- Flaky tests? Ask in #engineering channel
- Coverage issues? Review with tech lead
- CI failing? Check GitHub Actions logs

---

## Summary

**Testing is not optional. It's how we ensure quality.**

Every feature must have:
- ✅ Clear acceptance criteria
- ✅ Automated tests (unit, widget, integration)
- ✅ ≥80% code coverage
- ✅ Passing CI before merge

This strategy ensures:
- 🐛 Fewer bugs reach production
- 🚀 Faster development (confident refactoring)
- 📈 Higher quality product
- 😊 Happier users

**Quality is everyone's responsibility. Test your code!**

---

*This document is a living guide. Update it as we learn and improve our testing practices.*
