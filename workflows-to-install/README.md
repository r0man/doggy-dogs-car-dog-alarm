# GitHub Actions Workflows - Installation Instructions

This directory contains the complete GitHub Actions CI/CD workflow files for Issues 31, 32, 33, and 34.

## Recent Updates

**Latest**: Workflows have been updated with the following fixes:
- Updated `actions/cache` from v3 to v4 (deprecated action fix)
- Fixed cache path syntax (`${{ env.HOME }}` → `~`)
- Replaced deprecated `actions/create-release@v1` and `actions/upload-release-asset@v1` with modern `softprops/action-gh-release@v1`
- Fixed incorrect secret checking syntax (moved to env vars)
- Fixed wildcard path issues in iOS TestFlight deployment
- Fixed deprecated `flutter format` command → now using `dart format`
- Improved error handling with proper continue-on-error flags
- Added proper permissions declarations for release creation

These changes modernize the workflows, fix deprecation warnings, and improve build reliability.

## Why are they here?

Due to GitHub App permissions, Claude Code cannot directly push files to `.github/workflows/`.
This is a security feature to prevent unauthorized workflow modifications.

## Installation

To activate these workflows, you need to manually move them to `.github/workflows/`:

```bash
# From the repository root:
mkdir -p .github/workflows
cp workflows-to-install/test.yml .github/workflows/
cp workflows-to-install/build-android.yml .github/workflows/
cp workflows-to-install/build-ios.yml .github/workflows/
cp workflows-to-install/release.yml .github/workflows/

# Commit and push:
git add .github/workflows/
git commit -m "Add complete GitHub Actions CI/CD workflows (Issues 31-34)"
git push
```

## What's Included

### `test.yml` - Automated Testing Workflow (Issue-33)
- Runs on every PR and push to main/develop
- Code formatting check (`dart format`)
- Static analysis (`flutter analyze`)
- Test execution with coverage
- Enforces 80% coverage threshold
- Codecov integration (optional)
- PR comments with coverage report

**No setup required** - works immediately after installation!

### `build-android.yml` - Android Build Workflow (Issue-31)
- Builds debug APK for PRs
- Builds signed release APK for main/develop pushes
- Builds AAB for main branch only
- Includes graceful fallback if secrets not configured

**Optional setup** - For signed release builds, configure these GitHub Secrets:
- `ANDROID_KEYSTORE_BASE64`
- `KEYSTORE_PASSWORD`
- `KEY_ALIAS`
- `KEY_PASSWORD`

See `ANDROID_SIGNING_SETUP.md` for detailed instructions.

### `build-ios.yml` - iOS Build Workflow (Issue-32)
- Builds debug IPA for PRs (no signing)
- Builds signed release IPA for main/develop pushes
- Fastlane Match integration for code signing
- Optional TestFlight deployment
- macOS runner required

**Optional setup** - For signed release builds, configure these GitHub Secrets:
- `MATCH_GIT_URL` - SSH URL to certificates repo
- `MATCH_GIT_PRIVATE_KEY` - Private SSH key
- `MATCH_PASSWORD` - Match encryption passphrase
- `FASTLANE_USER` - Apple ID email
- `FASTLANE_APPLE_APPLICATION_SPECIFIC_PASSWORD` - App-specific password

See `IOS_SIGNING_SETUP.md` for detailed instructions.

### `release.yml` - Automated Release Workflow (Issue-34)
- Creates GitHub releases on version tags (v*.*.*)
- Auto-generates changelog from commits
- Builds and uploads Android APK + AAB
- Builds and uploads iOS IPA (if configured)
- Updates CHANGELOG.md automatically
- Supports semantic versioning

**Usage:**
```bash
# Create and push a version tag
git tag v1.0.0
git push origin v1.0.0

# Or use workflow_dispatch with manual version input
```

**No additional setup required** - uses existing Android/iOS secrets!

## Documentation

Complete documentation is available:
- `GITHUB_ACTIONS_SETUP.md` - Comprehensive workflow guide
- `ANDROID_SIGNING_SETUP.md` - Android signing setup guide
- `IOS_SIGNING_SETUP.md` - iOS signing with Fastlane Match
- `CI_CD_STRATEGY.md` - Overall CI/CD strategy and architecture

## Testing the Workflows

After installation:

1. **Test Workflow (`test.yml`)**: Runs automatically on every push and PR
2. **Android Build (`build-android.yml`)**: Runs on push to main/develop or PR to main
3. **iOS Build (`build-ios.yml`)**: Runs on push to main/develop or PR to main (requires macOS runner)
4. **Release (`release.yml`)**: Runs when you push a version tag (e.g., `v1.0.0`)

### Quick Test

```bash
# Push to trigger test and build workflows
git push

# Create a release (requires signed builds to be configured)
git tag v0.1.0
git push origin v0.1.0
```

## Branch Protection (Recommended)

After verifying the workflows work, configure branch protection rules:

**For `main` branch:**
- Require status checks: `Run Tests & Check Coverage`, `Quality Gate`
- Require branches to be up to date
- Require pull request reviews

See `GITHUB_ACTIONS_SETUP.md` for complete branch protection recommendations.

## Questions?

Refer to the troubleshooting sections in:
- `GITHUB_ACTIONS_SETUP.md` - General workflow documentation
- `ANDROID_SIGNING_SETUP.md` - Android signing and keystore setup
- `IOS_SIGNING_SETUP.md` - iOS signing with Fastlane Match
- `CI_CD_STRATEGY.md` - Overall architecture and strategy

## Workflow Summary

| Workflow | Trigger | Purpose | Secrets Required |
|----------|---------|---------|------------------|
| `test.yml` | PR, Push | Run tests, enforce coverage | None |
| `build-android.yml` | PR, Push to main/develop | Build Android APK/AAB | Optional (for signed builds) |
| `build-ios.yml` | PR, Push to main/develop | Build iOS IPA | Optional (for signed builds) |
| `release.yml` | Tag push (v*.*.*) | Create GitHub release with artifacts | Uses Android/iOS secrets |

---

**Note**: You can safely delete the `workflows-to-install/` directory after copying the files to `.github/workflows/`.
