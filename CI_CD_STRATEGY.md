# CI/CD Strategy for Doggy Dogs Car Alarm

**Date**: 2025-10-28
**Status**: Research Complete
**Related Issues**: Issue-30, 31, 32, 33, 34

---

## Table of Contents

1. [Overview](#overview)
2. [Architecture](#architecture)
3. [Workflow Structure](#workflow-structure)
4. [Android Build Pipeline](#android-build-pipeline)
5. [iOS Build Pipeline](#ios-build-pipeline)
6. [Automated Testing](#automated-testing)
7. [Release Management](#release-management)
8. [Security & Secrets](#security--secrets)
9. [Performance Optimization](#performance-optimization)
10. [Implementation Roadmap](#implementation-roadmap)

---

## Overview

This document outlines the CI/CD strategy for the Doggy Dogs Car Alarm Flutter application using GitHub Actions. The strategy prioritizes:

- **Quality**: Automated testing with coverage enforcement
- **Speed**: Caching and parallel builds
- **Security**: Proper secrets management
- **Automation**: Minimal manual intervention
- **Reliability**: Clear failure modes and notifications

---

## Architecture

### High-Level Pipeline

```
┌─────────────┐
│  Git Push   │
│   / PR      │
└──────┬──────┘
       │
       ├─────────────┐
       │             │
       v             v
┌──────────┐  ┌──────────┐
│  Tests   │  │ Analysis │
│ Coverage │  │  Format  │
└────┬─────┘  └────┬─────┘
     │             │
     └──────┬──────┘
            │ (if main/release)
            v
    ┌───────────────┐
    │     Build     │
    ├───────┬───────┤
    │Android│  iOS  │
    └───┬───┴───┬───┘
        │       │
        v       v
    ┌────┐  ┌──────┐
    │APK │  │ IPA  │
    │AAB │  │      │
    └─┬──┘  └───┬──┘
      │         │
      └────┬────┘
           │ (on tag)
           v
    ┌────────────┐
    │  Release   │
    │  • GitHub  │
    │  • Stores  │
    └────────────┘
```

### Workflow Files

Create the following in `.github/workflows/`:

1. **`test.yml`** - Run on every PR/push
2. **`build-android.yml`** - Build Android on main/release
3. **`build-ios.yml`** - Build iOS on main/release
4. **`release.yml`** - Create releases on tags

---

## Workflow Structure

### Common Setup Pattern

All workflows should follow this pattern:

```yaml
name: Workflow Name

on:
  # Triggers

env:
  FLUTTER_VERSION: '3.35.4'
  JAVA_VERSION: '21'

jobs:
  job-name:
    runs-on: ubuntu-latest  # or macos-latest for iOS

    steps:
      # 1. Checkout code
      # 2. Setup Java (Android only)
      # 3. Setup Flutter
      # 4. Cache dependencies
      # 5. Install dependencies
      # 6. Run tasks
      # 7. Upload artifacts
```

### Recommended Actions

| Purpose | Action | Version |
|---------|--------|---------|
| Checkout | `actions/checkout@v4` | Latest |
| Java Setup | `actions/setup-java@v4` | Latest |
| Flutter Setup | `subosito/flutter-action@v2` | Latest |
| Artifact Upload | `actions/upload-artifact@v4` | Latest |
| Code Coverage | `codecov/codecov-action@v4` | Latest |
| Release Creation | `softprops/action-gh-release@v1` | Latest |

---

## Android Build Pipeline

### Prerequisites

**Required Secrets:**
- `ANDROID_KEYSTORE_BASE64` - Base64 encoded keystore file
- `KEYSTORE_PASSWORD` - Keystore password
- `KEY_ALIAS` - Key alias
- `KEY_PASSWORD` - Key password

### Build Process

#### Step 1: Prepare Signing Configuration

```bash
# Locally, prepare your keystore:
keytool -genkey -v -keystore release.keystore \
  -alias release -keyalg RSA -keysize 2048 -validity 10000

# Base64 encode it:
openssl base64 < release.keystore | tr -d '\n' > keystore.base64.txt

# Add to GitHub Secrets: ANDROID_KEYSTORE_BASE64
```

#### Step 2: Workflow Configuration

```yaml
- name: Decode Keystore
  run: |
    echo "${{ secrets.ANDROID_KEYSTORE_BASE64 }}" | base64 --decode > android/app/keystore.jks

- name: Create key.properties
  run: |
    cat > android/key.properties <<EOF
    storePassword=${{ secrets.KEYSTORE_PASSWORD }}
    keyPassword=${{ secrets.KEY_PASSWORD }}
    keyAlias=${{ secrets.KEY_ALIAS }}
    storeFile=keystore.jks
    EOF

- name: Build APK
  run: flutter build apk --release

- name: Build AAB
  run: flutter build appbundle --release
```

#### Step 3: Configure `android/app/build.gradle`

```gradle
def keystoreProperties = new Properties()
def keystorePropertiesFile = rootProject.file('key.properties')
if (keystorePropertiesFile.exists()) {
    keystoreProperties.load(new FileInputStream(keystorePropertiesFile))
}

android {
    ...

    signingConfigs {
        release {
            keyAlias keystoreProperties['keyAlias']
            keyPassword keystoreProperties['keyPassword']
            storeFile keystoreProperties['storeFile'] ? file(keystoreProperties['storeFile']) : null
            storePassword keystoreProperties['storePassword']
        }
    }

    buildTypes {
        release {
            signingConfig signingConfigs.release
        }
    }
}
```

### Outputs

- **APK**: `build/app/outputs/flutter-apk/app-release.apk`
- **AAB**: `build/app/outputs/bundle/release/app-release.aab`

### Caching Strategy

```yaml
- name: Cache Gradle
  uses: actions/cache@v3
  with:
    path: |
      ~/.gradle/caches
      ~/.gradle/wrapper
    key: ${{ runner.os }}-gradle-${{ hashFiles('**/*.gradle*', '**/gradle-wrapper.properties') }}
```

---

## iOS Build Pipeline

### Prerequisites

**Required Tools:**
- Fastlane installed
- Apple Developer Account
- App Store Connect API Key
- Fastlane Match repository (private)

**Required Secrets:**
- `APP_STORE_CONNECT_API_KEY_ID`
- `APP_STORE_CONNECT_ISSUER_ID`
- `APP_STORE_CONNECT_API_KEY_CONTENT` (Base64 encoded .p8 file)
- `MATCH_PASSWORD` - Encryption password for Match
- `MATCH_GIT_BASIC_AUTHORIZATION` - Token for Match repo access
- `MATCH_GIT_URL` - URL to certificates repository

### Setup Fastlane Match

#### 1. Initialize Match

```bash
cd ios
fastlane match init

# Choose 'git' storage
# Enter private repo URL for certificates
```

#### 2. Generate Certificates

```bash
# Development
fastlane match development

# App Store distribution
fastlane match appstore
```

#### 3. Create Fastfile

```ruby
# ios/fastlane/Fastfile
default_platform(:ios)

platform :ios do
  desc "Build and upload to TestFlight"
  lane :beta do
    setup_ci if ENV['CI']

    match(
      type: "appstore",
      readonly: is_ci,
      git_basic_authorization: ENV["MATCH_GIT_BASIC_AUTHORIZATION"]
    )

    build_app(
      scheme: "Runner",
      export_method: "app-store",
      output_directory: "./build",
      output_name: "app.ipa"
    )

    upload_to_testflight(
      api_key_path: ENV["APP_STORE_CONNECT_API_KEY_PATH"],
      skip_waiting_for_build_processing: true
    )
  end
end
```

### Workflow Configuration

```yaml
jobs:
  build-ios:
    runs-on: macos-latest

    steps:
      - uses: actions/checkout@v4

      - uses: subosito/flutter-action@v2
        with:
          flutter-version: '3.35.4'

      - name: Install dependencies
        run: flutter pub get

      - name: Create API Key JSON
        run: |
          echo "${{ secrets.APP_STORE_CONNECT_API_KEY_CONTENT }}" | base64 --decode > /tmp/api_key.p8

      - name: Build iOS
        env:
          MATCH_PASSWORD: ${{ secrets.MATCH_PASSWORD }}
          MATCH_GIT_BASIC_AUTHORIZATION: ${{ secrets.MATCH_GIT_BASIC_AUTHORIZATION }}
          APP_STORE_CONNECT_API_KEY_PATH: /tmp/api_key.p8
        run: |
          cd ios
          fastlane beta
```

### Outputs

- **IPA**: `ios/build/app.ipa`
- **dSYM**: `ios/build/app.app.dSYM.zip`

---

## Automated Testing

### Test Workflow

```yaml
name: Tests

on:
  pull_request:
  push:
    branches: [main, develop]

jobs:
  test:
    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v4

      - uses: subosito/flutter-action@v2
        with:
          flutter-version: '3.35.4'
          channel: 'stable'
          cache: true

      - name: Cache pub dependencies
        uses: actions/cache@v3
        with:
          path: ${{ env.PUB_CACHE }}
          key: ${{ runner.os }}-pub-${{ hashFiles('**/pubspec.lock') }}
          restore-keys: |
            ${{ runner.os }}-pub-

      - name: Install dependencies
        run: flutter pub get

      - name: Verify formatting
        run: flutter format --set-exit-if-changed .

      - name: Analyze project source
        run: flutter analyze

      - name: Run tests with coverage
        run: flutter test --coverage

      - name: Check coverage threshold
        run: |
          COVERAGE=$(lcov --summary coverage/lcov.info | grep -oP 'lines......: \K[0-9.]+')
          echo "Coverage: $COVERAGE%"
          if (( $(echo "$COVERAGE < 80.0" | bc -l) )); then
            echo "Coverage below 80%!"
            exit 1
          fi

      - name: Upload coverage to Codecov
        uses: codecov/codecov-action@v4
        with:
          file: ./coverage/lcov.info
          token: ${{ secrets.CODECOV_TOKEN }}
          fail_ci_if_error: true
```

### Coverage Configuration

Create `coverage_helper.dart` to include all files:

```dart
// test/coverage_helper.dart
// ignore_for_file: unused_import

// Import all library files to ensure coverage
import 'package:doggy_dogs_car_alarm/main.dart';
import 'package:doggy_dogs_car_alarm/models/dog.dart';
import 'package:doggy_dogs_car_alarm/models/sensor_data.dart';
// ... import all other files
```

### Coverage Thresholds

- **Minimum**: 80% overall coverage
- **Critical paths**: 90% coverage (alarm, sensors, dog provider)
- **UI code**: 70% coverage (screens, widgets)

### Quality Gates

All PRs must pass:
1. ✅ Code formatting (`flutter format`)
2. ✅ Static analysis (`flutter analyze`)
3. ✅ All tests passing
4. ✅ Coverage >= 80%

---

## Release Management

### Versioning Strategy

**Follow Semantic Versioning (SemVer):**

```
MAJOR.MINOR.PATCH+BUILD

Example: 1.2.3+10
  ├─ 1: Major version (breaking changes)
  ├─ 2: Minor version (new features)
  ├─ 3: Patch version (bug fixes)
  └─ 10: Build number (auto-increment)
```

### Version Management

Update `pubspec.yaml`:

```yaml
version: 1.0.0+1
```

### Conventional Commits

Use conventional commit messages for automated changelog:

```
feat: Add breed selection screen
fix: Resolve sensor calibration issue
docs: Update CI/CD documentation
chore: Bump Flutter version
test: Add tests for dog provider
```

### Release Workflow

```yaml
name: Release

on:
  push:
    tags:
      - 'v*'

jobs:
  release:
    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v4
        with:
          fetch-depth: 0  # Need full history for changelog

      - name: Generate Changelog
        id: changelog
        run: |
          PREVIOUS_TAG=$(git describe --abbrev=0 --tags $(git rev-list --tags --skip=1 --max-count=1) 2>/dev/null || echo "")
          if [ -z "$PREVIOUS_TAG" ]; then
            COMMITS=$(git log --pretty=format:"* %s (%h)" ${{ github.ref_name }})
          else
            COMMITS=$(git log --pretty=format:"* %s (%h)" $PREVIOUS_TAG..${{ github.ref_name }})
          fi
          echo "changelog<<EOF" >> $GITHUB_OUTPUT
          echo "$COMMITS" >> $GITHUB_OUTPUT
          echo "EOF" >> $GITHUB_OUTPUT

      - name: Create GitHub Release
        uses: softprops/action-gh-release@v1
        with:
          body: |
            ## What's Changed

            ${{ steps.changelog.outputs.changelog }}

            ## Downloads

            - Android APK: See assets below
            - iOS IPA: Available on TestFlight
          files: |
            build/app/outputs/flutter-apk/app-release.apk
            build/app/outputs/bundle/release/app-release.aab
          token: ${{ secrets.GITHUB_TOKEN }}
```

### Release Process

1. **Update version** in `pubspec.yaml`
2. **Commit** version bump: `git commit -m "chore: bump version to 1.2.0"`
3. **Create tag**: `git tag v1.2.0`
4. **Push tag**: `git push origin v1.2.0`
5. **GitHub Actions** creates release automatically

---

## Security & Secrets

### Required Secrets

Configure in GitHub Settings → Secrets → Actions:

#### Android
- `ANDROID_KEYSTORE_BASE64`
- `KEYSTORE_PASSWORD`
- `KEY_ALIAS`
- `KEY_PASSWORD`

#### iOS
- `APP_STORE_CONNECT_API_KEY_ID`
- `APP_STORE_CONNECT_ISSUER_ID`
- `APP_STORE_CONNECT_API_KEY_CONTENT`
- `MATCH_PASSWORD`
- `MATCH_GIT_BASIC_AUTHORIZATION`
- `MATCH_GIT_URL`

#### Testing & Coverage
- `CODECOV_TOKEN` (for private repos)

### Best Practices

1. **Never commit secrets** to version control
2. **Rotate secrets** regularly (every 90 days)
3. **Use environment-specific** secrets where possible
4. **Limit secret access** to necessary workflows only
5. **Audit secret usage** in workflow runs

### Keystore Security

- Store keystore in secure location
- Keep backup of keystore (losing it means can't update app)
- Document recovery process
- Use strong passwords (min 16 characters)

---

## Performance Optimization

### Caching Strategy

#### 1. Flutter SDK Cache

```yaml
- uses: subosito/flutter-action@v2
  with:
    flutter-version: '3.35.4'
    cache: true  # Enables built-in caching
```

#### 2. Pub Dependencies Cache

```yaml
- name: Cache pub dependencies
  uses: actions/cache@v3
  with:
    path: ~/.pub-cache
    key: ${{ runner.os }}-pub-${{ hashFiles('**/pubspec.lock') }}
    restore-keys: |
      ${{ runner.os }}-pub-
```

#### 3. Gradle Cache (Android)

```yaml
- name: Cache Gradle
  uses: actions/cache@v3
  with:
    path: |
      ~/.gradle/caches
      ~/.gradle/wrapper
    key: ${{ runner.os }}-gradle-${{ hashFiles('**/*.gradle*') }}
```

#### 4. CocoaPods Cache (iOS)

```yaml
- name: Cache CocoaPods
  uses: actions/cache@v3
  with:
    path: ios/Pods
    key: ${{ runner.os }}-pods-${{ hashFiles('**/Podfile.lock') }}
```

### Expected Performance

With caching:
- **Test workflow**: 2-3 minutes
- **Android build**: 8-12 minutes
- **iOS build**: 15-20 minutes

Without caching:
- **Test workflow**: 5-6 minutes
- **Android build**: 15-20 minutes
- **iOS build**: 25-30 minutes

### Parallel Builds

Use matrix strategy for multi-platform:

```yaml
strategy:
  matrix:
    platform: [android, ios]
  fail-fast: false  # Continue other builds if one fails
```

---

## Implementation Roadmap

### Phase 1: Testing & Quality (Week 1)
**Issue-33: Automated Testing**

- [x] Research complete
- [ ] Create `.github/workflows/test.yml`
- [ ] Add code formatting check
- [ ] Add static analysis
- [ ] Add test coverage reporting
- [ ] Integrate with Codecov
- [ ] Enforce 80% coverage threshold
- [ ] Test on PRs

**Deliverables:**
- Working test workflow
- Coverage reporting
- Quality gates enforced

---

### Phase 2: Android Build (Week 2)
**Issue-31: Android Build Workflow**

- [x] Research complete
- [ ] Generate release keystore
- [ ] Configure signing in `build.gradle`
- [ ] Add secrets to GitHub
- [ ] Create `.github/workflows/build-android.yml`
- [ ] Test APK build
- [ ] Test AAB build
- [ ] Verify signing
- [ ] Add artifact upload

**Deliverables:**
- Signed APK builds
- Signed AAB builds
- Automated on main branch

---

### Phase 3: Release Automation (Week 3)
**Issue-34: Release Workflow**

- [x] Research complete
- [ ] Implement version management strategy
- [ ] Create `.github/workflows/release.yml`
- [ ] Add changelog generation
- [ ] Configure artifact uploads
- [ ] Test with beta release
- [ ] Document release process

**Deliverables:**
- Automated releases on tags
- Generated changelogs
- APK/AAB artifacts attached

---

### Phase 4: iOS Build (Optional)
**Issue-32: iOS Build Workflow**

- [x] Research complete
- [ ] Set up Apple Developer account
- [ ] Configure App Store Connect API
- [ ] Initialize Fastlane Match
- [ ] Create certificates repository
- [ ] Configure Fastlane
- [ ] Create `.github/workflows/build-ios.yml`
- [ ] Test TestFlight upload

**Deliverables:**
- IPA builds
- TestFlight deployment
- Automated on main branch

**Note**: iOS builds require:
- Apple Developer Program membership ($99/year)
- macOS runner (costs more in GitHub Actions)
- More complex setup

Can defer until Android is fully working.

---

## Monitoring & Maintenance

### Workflow Health

Monitor workflow runs in GitHub Actions:
- Check success rate
- Track build times
- Review failure patterns
- Optimize slow steps

### Notifications

Configure notifications for:
- Failed builds on main/develop
- Failed releases
- Coverage drops below threshold

### Maintenance Tasks

**Monthly:**
- Review and update Flutter version
- Check for security updates
- Rotate secrets if needed
- Review caching effectiveness

**Quarterly:**
- Audit workflow performance
- Update action versions
- Review secret usage
- Optimize build times

---

## Next Steps

1. **Start with Issue-33** (Testing workflow) - Highest value, lowest risk
2. **Then Issue-31** (Android builds) - Core platform
3. **Then Issue-34** (Releases) - Automation complete
4. **Finally Issue-32** (iOS builds) - Optional enhancement

---

## Resources

### Documentation
- [Flutter CI/CD Guide](https://docs.flutter.dev/deployment/cd)
- [GitHub Actions for Flutter](https://github.com/subosito/flutter-action)
- [Fastlane for Flutter](https://docs.fastlane.tools/)
- [Codecov Documentation](https://docs.codecov.com/)

### Example Repositories
- [Flutter Gallery CI/CD](https://github.com/flutter/gallery)
- [Very Good Ventures Templates](https://github.com/VeryGoodOpenSource)

### Tools
- [Cider](https://pub.dev/packages/cider) - Versioning automation
- [Fastlane](https://fastlane.tools/) - iOS/Android deployment
- [semantic-release](https://semantic-release.gitbook.io/) - Automated versioning

---

## Conclusion

This CI/CD strategy provides a comprehensive, production-ready approach for automating builds, tests, and releases of the Doggy Dogs Car Alarm Flutter application. The phased implementation allows incremental value delivery while managing complexity and costs.

**Estimated Timeline**: 3-4 weeks for full implementation (excluding iOS)

**Key Benefits:**
- ✅ Automated quality checks on every PR
- ✅ Consistent, repeatable builds
- ✅ Reduced manual release work
- ✅ Faster feedback cycles
- ✅ Better code quality through enforcement
