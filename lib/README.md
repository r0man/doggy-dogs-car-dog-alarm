# Flutter Project Structure

This document describes the organization of the Flutter app codebase.

## Directory Structure

```
lib/
├── main.dart                 # App entry point
├── models/                   # Data models
│   └── dog.dart             # Dog, DogBreed, DogStats, DogPersonality models
├── screens/                  # Full-screen UI pages
│   └── home_screen.dart     # Main home screen
├── widgets/                  # Reusable UI components
├── services/                 # Business logic & data services
└── utils/                    # Helper functions & constants
```

## Conventions

### File Naming
- Use `snake_case` for file names (e.g., `home_screen.dart`)
- Use `PascalCase` for class names (e.g., `HomeScreen`)

### Code Organization

#### Models (`models/`)
- Data classes representing app entities
- Immutable where possible (use `copyWith()` for updates)
- Include serialization methods when needed

#### Screens (`screens/`)
- Full-page views
- Typically StatefulWidget or ConsumerStatefulWidget (Riverpod)
- Handle routing and navigation
- Should be lightweight, delegate business logic to services

#### Widgets (`widgets/`)
- Reusable UI components
- Keep them small and focused
- Make them configurable through constructor parameters

#### Services (`services/`)
- Business logic
- API calls
- Database operations
- Sensor management
- Background processing

#### Utils (`utils/`)
- Constants
- Helper functions
- Extensions
- Validators

## State Management

This project uses **Riverpod** for state management:
- Providers are defined in service files
- Screens/widgets consume providers using `ConsumerWidget` or `ConsumerStatefulWidget`
- Use `.ref.watch()` for listening to state changes
- Use `.ref.read()` for one-time reads

## Testing

Tests are located in the `test/` directory and mirror the `lib/` structure:
```
test/
├── models/
│   └── dog_test.dart
├── screens/
├── widgets/
└── services/
```

Run tests with:
```bash
flutter test
```

## Assets

Assets are organized in the `assets/` directory:
```
assets/
├── dogs/          # Dog breed SVG artwork
└── sounds/        # Bark sounds and audio effects
```

Reference assets in code:
```dart
'assets/dogs/german_shepherd.svg'
'assets/sounds/bark_aggressive.mp3'
```
