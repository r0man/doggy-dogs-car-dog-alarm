# GitHub Actions CI/CD Setup Guide

This document provides a comprehensive guide to the GitHub Actions workflows implemented for the Doggy Dogs Car Alarm project.

---

## Overview

We have implemented two main workflows:

1. **Test Workflow** (`test.yml`) - Runs on every PR and push
2. **Android Build Workflow** (`build-android.yml`) - Builds APK/AAB on main/develop

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

### Adding More Platforms

To add iOS builds, create `.github/workflows/build-ios.yml` following the pattern in `build-android.yml`. See [CI_CD_STRATEGY.md](./CI_CD_STRATEGY.md) for iOS setup details.

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

1. ✅ Workflows are set up
2. [ ] Configure Android signing secrets
3. [ ] Test workflow on a PR
4. [ ] Set up branch protection rules
5. [ ] Add status badges to README
6. [ ] Configure Codecov (optional)
7. [ ] Document team workflow

---

## Resources

- [GitHub Actions Documentation](https://docs.github.com/en/actions)
- [Flutter CI/CD Guide](https://docs.flutter.dev/deployment/cd)
- [subosito/flutter-action](https://github.com/subosito/flutter-action)
- [CI_CD_STRATEGY.md](./CI_CD_STRATEGY.md) - Full strategy document
- [ANDROID_SIGNING_SETUP.md](./ANDROID_SIGNING_SETUP.md) - Signing setup

---

**Questions?** Check the troubleshooting section or review workflow logs in the Actions tab.
