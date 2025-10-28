# GitHub Actions CI/CD Setup Guide

This document provides a comprehensive guide to the GitHub Actions workflows implemented for the Doggy Dogs Car Alarm project.

---

## Overview

We have implemented four comprehensive CI/CD workflows:

1. **Test Workflow** (`test.yml`) - Automated testing with coverage enforcement
2. **Android Build Workflow** (`build-android.yml`) - Android APK/AAB builds
3. **iOS Build Workflow** (`build-ios.yml`) - iOS IPA builds with Fastlane Match
4. **Release Workflow** (`release.yml`) - Automated releases with semantic versioning

---

## Workflow 1: Automated Testing

**File**: `.github/workflows/test.yml`

### Triggers

- **Pull Requests** to `main` or `develop`
- **Pushes** to `main` or `develop`

### What It Does

1. ✅ **Code Formatting Check** - Ensures consistent code style
2. ✅ **Static Analysis** - Catches potential bugs and issues
3. ✅ **Unit Tests** - Runs all 65 tests
4. ✅ **Coverage Check** - Enforces 80% minimum coverage
5. ✅ **Coverage Upload** - Sends coverage to Codecov (optional)
6. ✅ **PR Comments** - Posts coverage report on pull requests

### Quality Gates

All PRs must pass:
- Code formatting (`flutter format`)
- Static analysis (`flutter analyze`)
- All tests must pass
- Coverage ≥ 80%

### Setup Requirements

**Required**:
- None! Works out of the box

**Optional** (for Codecov):
- Add `CODECOV_TOKEN` secret (for private repos)
- Sign up at [codecov.io](https://codecov.io)

### Example Output

```
✅ Code formatting check passed
✅ Static analysis passed
✅ All 65 tests passed
✅ Coverage: 85.3% (threshold: 80%)
```

---

## Workflow 2: Android Builds

**File**: `.github/workflows/build-android.yml`

### Triggers

- **Pushes** to `main` or `develop` branches
- **Pull Requests** to `main`
- **Manual dispatch** (from Actions tab)

### What It Does

#### On Pull Requests:
- Builds **debug APK** for testing
- No signing required

#### On Push to main/develop:
- Decodes keystore from secrets
- Creates signing configuration
- Builds **signed release APK**
- Builds **signed AAB** (main branch only)
- Uploads artifacts

### Build Matrix

| Branch | PR | Push |
|--------|----|----|
| `main` | Debug APK | Release APK + AAB |
| `develop` | Debug APK | Release APK |
| Other | Debug APK | - |

### Setup Requirements

#### Required Secrets:

Configure in **Settings → Secrets → Actions**:

1. `ANDROID_KEYSTORE_BASE64` - Base64 encoded keystore file
2. `KEYSTORE_PASSWORD` - Keystore password
3. `KEY_ALIAS` - Key alias (usually "release")
4. `KEY_PASSWORD` - Key password

See [ANDROID_SIGNING_SETUP.md](./ANDROID_SIGNING_SETUP.md) for detailed setup instructions.

#### First Time Setup:

```bash
# 1. Generate keystore
keytool -genkey -v -keystore release.keystore \
  -alias release -keyalg RSA -keysize 2048 -validity 10000

# 2. Encode to base64
openssl base64 < release.keystore | tr -d '\n' > keystore.base64.txt

# 3. Add to GitHub Secrets (paste contents of keystore.base64.txt)
```

### Artifacts

After successful build, download artifacts from the workflow run:

- **android-apk-{version}** - Contains APK file(s)
- **android-aab-{version}** - Contains AAB file (main branch only)

### Example Output

```
📱 Android Build Summary

Version: 1.0.0+1
Branch: main
Commit: abc123...

APK: Release (45.2 MB)
AAB: Release (42.8 MB)

✅ Build completed successfully!
```

---

## Caching Strategy

Both workflows use aggressive caching to speed up builds:

### Flutter SDK Cache
- Cached by `subosito/flutter-action`
- Saves ~2 minutes per run

### Pub Dependencies Cache
- Key: OS + pubspec.lock hash
- Saves ~30 seconds per run

### Gradle Cache (Android builds)
- Caches Gradle wrapper and dependencies
- Saves ~3-5 minutes per run

### Expected Build Times

| Workflow | First Run | Cached Run |
|----------|-----------|------------|
| Tests | 5-6 min | 2-3 min |
| Android Build | 15-20 min | 8-12 min |
| iOS Build | 25-30 min | 15-20 min |
| Release (All platforms) | 35-45 min | 20-30 min |

---

## Branch Protection Rules

### Recommended Settings

Configure in **Settings → Branches → Branch protection rules**:

#### For `main` branch:

- [x] Require status checks to pass before merging
  - [x] `Run Tests & Check Coverage`
  - [x] `Quality Gate`
- [x] Require branches to be up to date
- [x] Require pull request reviews (1+)
- [x] Dismiss stale reviews
- [x] Require linear history
- [ ] Allow force pushes (disabled)
- [ ] Allow deletions (disabled)

#### For `develop` branch:

- [x] Require status checks to pass before merging
  - [x] `Run Tests & Check Coverage`
- [x] Require branches to be up to date
- [ ] Require pull request reviews (optional for development)

---

## Monitoring & Maintenance

### Weekly Checklist

- [ ] Review failed workflow runs
- [ ] Check build times (optimize if > 15 min)
- [ ] Monitor artifact storage usage
- [ ] Review coverage trends

### Monthly Checklist

- [ ] Update Flutter version if new stable release
- [ ] Update GitHub Actions to latest versions
- [ ] Review and clean old artifacts
- [ ] Check secret expiration dates

### Quarterly Checklist

- [ ] Rotate signing secrets (optional but recommended)
- [ ] Audit workflow permissions
- [ ] Review caching effectiveness
- [ ] Update documentation

---

## Troubleshooting

### Test Workflow Failures

#### "Code formatting check failed"
```bash
# Fix locally:
flutter format .

# Commit and push
git add .
git commit -m "chore: format code"
git push
```

#### "Static analysis failed"
```bash
# Check issues:
flutter analyze

# Fix the reported issues and re-run
```

#### "Tests failed"
```bash
# Run tests locally:
flutter test

# Debug specific test:
flutter test test/path/to/test_test.dart
```

#### "Coverage below threshold"
```bash
# Check current coverage:
flutter test --coverage
lcov --summary coverage/lcov.info

# Add more tests to increase coverage
```

### Android Build Failures

#### "Keystore not found"
- Check `ANDROID_KEYSTORE_BASE64` secret is set
- Verify no extra whitespace in secret value
- Re-encode keystore if needed

#### "Gradle build failed"
- Check Java version (should be 17)
- Clear Gradle cache and retry
- Check `build.gradle` syntax

#### "Signing failed"
- Verify all 4 secrets are set correctly
- Test signing locally first
- Check keystore hasn't expired (10000 days validity)

### General Issues

#### "Workflow not triggering"
- Check branch name matches trigger conditions
- Verify workflow file is in `.github/workflows/`
- Check YAML syntax is valid

#### "Workflow runs but steps fail"
- Check workflow logs for specific error
- Test same commands locally
- Verify Flutter/Java versions match

---

## Advanced Configuration

### Running Workflows Manually

You can manually trigger the Android build workflow:

1. Go to **Actions** tab
2. Select **Build Android** workflow
3. Click **Run workflow** button
4. Choose branch and click **Run workflow**

### Customizing Coverage Threshold

Edit `.github/workflows/test.yml`:

```yaml
# Change this line:
THRESHOLD=80.0

# To your desired threshold:
THRESHOLD=85.0
```

### Workflow 3: iOS Builds

**File**: `.github/workflows/build-ios.yml`

See [IOS_SIGNING_SETUP.md](./IOS_SIGNING_SETUP.md) for complete iOS setup documentation.

#### Triggers
- **Pushes** to `main` or `develop` branches
- **Pull Requests** to `main`
- **Manual dispatch**

#### What It Does

**On Pull Requests:**
- Builds **debug IPA** (unsigned) for testing

**On Push to main/develop:**
- Sets up Fastlane and Ruby
- Syncs provisioning profiles with Fastlane Match
- Builds **signed release IPA**
- Uploads to TestFlight (optional, main branch only)

#### Setup Requirements

**Required Secrets** (for signed builds):

1. `MATCH_GIT_URL` - SSH URL to certificates repository
2. `MATCH_GIT_PRIVATE_KEY` - Private SSH key for accessing certificates repo
3. `MATCH_PASSWORD` - Encryption passphrase for Match
4. `FASTLANE_USER` - Apple ID email
5. `FASTLANE_APPLE_APPLICATION_SPECIFIC_PASSWORD` - App-specific password

See [IOS_SIGNING_SETUP.md](./IOS_SIGNING_SETUP.md) for step-by-step setup.

#### Build Matrix

| Branch | PR | Push |
|--------|----|----|
| `main` | Debug IPA | Signed IPA + TestFlight |
| `develop` | Debug IPA | Signed IPA |
| Other | Debug IPA | - |

#### macOS Runner Requirements

- iOS builds require macOS runners
- Free for public repos
- 2,000 minutes/month for private repos
- macOS minutes count as 10x Linux minutes

### Workflow 4: Automated Releases

**File**: `.github/workflows/release.yml`

#### Triggers
- **Tag push** matching `v*.*.*` (e.g., v1.0.0, v2.1.3)
- **Manual dispatch** with version input

#### What It Does

1. **Creates GitHub Release**
   - Auto-generates changelog from commits
   - Categorizes changes (Features, Bug Fixes, Other Changes)
   - Links to full changelog comparison

2. **Builds Android Artifacts**
   - Signed APK
   - Signed AAB (for Play Store)
   - Uploads to release

3. **Builds iOS Artifact**
   - Signed IPA (if configured)
   - Uploads to release

4. **Updates CHANGELOG.md**
   - Commits updated changelog to main branch
   - Follows Keep a Changelog format

#### Usage

**Create a release:**

```bash
# Ensure you're on main and up-to-date
git checkout main
git pull

# Create and push version tag
git tag v1.0.0
git push origin v1.0.0

# Or using annotated tag with message
git tag -a v1.0.0 -m "Release version 1.0.0"
git push origin v1.0.0
```

The workflow will automatically:
- Build all platforms
- Create GitHub release
- Upload artifacts
- Update CHANGELOG.md

**Manual trigger:**

1. Go to **Actions** tab
2. Select **Create Release** workflow
3. Click **Run workflow**
4. Enter version (e.g., 1.0.0)
5. Click **Run workflow**

#### Semantic Versioning

Follow [semver.org](https://semver.org/) guidelines:

- **MAJOR** (v2.0.0): Breaking changes
- **MINOR** (v1.1.0): New features, backwards compatible
- **PATCH** (v1.0.1): Bug fixes, backwards compatible

**Pre-release versions:**
- `v1.0.0-alpha.1` - Alpha release
- `v1.0.0-beta.2` - Beta release
- `v1.0.0-rc.1` - Release candidate

Pre-releases are marked as "pre-release" on GitHub automatically.

#### Commit Message Conventions

For better changelogs, use conventional commits:

- `feat: add jazz rhythm mini-game` → New Features
- `fix: resolve sensor detection issue` → Bug Fixes
- `docs: update README` → Other Changes
- `chore: update dependencies` → Other Changes

#### Setup Requirements

**No additional secrets required!**

The release workflow uses existing Android and iOS secrets. If signing is not configured, it will build unsigned artifacts.

**Optional:**
- Configure Android signing for signed APK/AAB
- Configure iOS signing for signed IPA

### Parallel Builds

Current setup runs:
- Tests on every PR/push
- Android builds only on main/develop pushes

To run tests and builds in parallel on main:

```yaml
# In test.yml
on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main, develop ]
```

Both workflows will run simultaneously.

---

## Workflow Status Badges

Add to your README.md:

```markdown
## Build Status

![Tests](https://github.com/yourusername/doggy-dogs-car-dog-alarm/workflows/Flutter%20Tests/badge.svg)
![Android Build](https://github.com/yourusername/doggy-dogs-car-dog-alarm/workflows/Build%20Android/badge.svg)
![iOS Build](https://github.com/yourusername/doggy-dogs-car-dog-alarm/workflows/Build%20iOS/badge.svg)
![Release](https://github.com/yourusername/doggy-dogs-car-dog-alarm/workflows/Create%20Release/badge.svg)
[![codecov](https://codecov.io/gh/yourusername/doggy-dogs-car-dog-alarm/branch/main/graph/badge.svg)](https://codecov.io/gh/yourusername/doggy-dogs-car-dog-alarm)
```

---

## Cost Considerations

### GitHub Actions Minutes

**Free tier** (Public repos):
- Unlimited minutes for public repos

**Free tier** (Private repos):
- 2,000 minutes/month
- Our workflows use ~5-15 min per run

**Cost optimization**:
- Caching reduces build time by 50%
- Only build on main/develop (not feature branches)
- Artifact retention: 30 days (APK), 90 days (AAB)

### Artifact Storage

**Free tier**:
- 500 MB for private repos
- Unlimited for public repos

**Our usage**:
- APK: ~45 MB per build
- AAB: ~43 MB per build
- Coverage report: ~1 MB

**Cost optimization**:
- 30-day retention for APKs
- 90-day retention for AABs (for releases)
- Coverage reports are small

---

## Security

### Secrets Management

✅ **Good Practices**:
- All sensitive data in GitHub Secrets
- Secrets never appear in logs
- Limited scope (only necessary workflows)
- Regular rotation recommended

❌ **Never Do**:
- Commit secrets to repository
- Echo/print secrets in workflows
- Share secrets outside team
- Reuse secrets across projects

### Permissions

Workflows have minimal permissions:
- Read repository content
- Write to artifacts
- Comment on PRs (tests only)

To review/modify:
```yaml
permissions:
  contents: read
  actions: read
  pull-requests: write  # For PR comments
```

---

## Migration Guide

### From Other CI Systems

#### From Travis CI:
- `.travis.yml` → `.github/workflows/test.yml`
- Environment variables → GitHub Secrets
- Build stages → Separate jobs/workflows

#### From CircleCI:
- `.circleci/config.yml` → `.github/workflows/*.yml`
- Workflows → Jobs
- Context → Secrets

#### From Jenkins:
- Jenkinsfile → Workflow YAML
- Credentials → GitHub Secrets
- Stages → Jobs and steps

---

## Next Steps

1. ✅ Workflows are set up (test, Android, iOS, release)
2. [ ] Configure Android signing secrets (see ANDROID_SIGNING_SETUP.md)
3. [ ] Configure iOS signing with Fastlane Match (see IOS_SIGNING_SETUP.md)
4. [ ] Test workflows on a PR
5. [ ] Set up branch protection rules
6. [ ] Add status badges to README
7. [ ] Configure Codecov (optional)
8. [ ] Create first release (git tag v0.1.0)
9. [ ] Document team workflow

---

## Resources

**Official Documentation:**
- [GitHub Actions Documentation](https://docs.github.com/en/actions)
- [Flutter CI/CD Guide](https://docs.flutter.dev/deployment/cd)
- [Fastlane Documentation](https://docs.fastlane.tools/)
- [Fastlane Match](https://docs.fastlane.tools/actions/match/)

**GitHub Actions:**
- [subosito/flutter-action](https://github.com/subosito/flutter-action)
- [ruby/setup-ruby](https://github.com/ruby/setup-ruby)
- [actions/create-release](https://github.com/actions/create-release)

**Project Documentation:**
- [CI_CD_STRATEGY.md](./CI_CD_STRATEGY.md) - Complete CI/CD strategy and architecture
- [ANDROID_SIGNING_SETUP.md](./ANDROID_SIGNING_SETUP.md) - Android keystore and signing
- [IOS_SIGNING_SETUP.md](./IOS_SIGNING_SETUP.md) - iOS signing with Fastlane Match
- [GITHUB_ACTIONS_SETUP.md](./GITHUB_ACTIONS_SETUP.md) - This document

---

**Questions?** Check the troubleshooting section or review workflow logs in the Actions tab.
