# GitHub Actions Workflows - Installation Instructions

This directory contains the GitHub Actions workflow files for Issues 31 and 33.

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

# Commit and push:
git add .github/workflows/
git commit -m "Add GitHub Actions workflows for CI/CD (Issues 31 & 33)"
git push
```

## What's Included

### `test.yml` - Automated Testing Workflow (Issue-33)
- Runs on every PR and push to main/develop
- Code formatting check (`flutter format`)
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

## Documentation

Complete documentation is available:
- `GITHUB_ACTIONS_SETUP.md` - Comprehensive workflow guide
- `ANDROID_SIGNING_SETUP.md` - Android signing setup guide

## Testing the Workflows

After installation:

1. **Test Workflow**: Will run automatically on your next push
2. **Build Workflow**: Will run when you push to main/develop or create a PR to main

## Branch Protection (Recommended)

After verifying the workflows work, configure branch protection rules:

**For `main` branch:**
- Require status checks: `Run Tests & Check Coverage`, `Quality Gate`
- Require branches to be up to date
- Require pull request reviews

See `GITHUB_ACTIONS_SETUP.md` for complete branch protection recommendations.

## Questions?

Refer to the troubleshooting sections in:
- `GITHUB_ACTIONS_SETUP.md`
- `ANDROID_SIGNING_SETUP.md`

---

**Note**: You can safely delete the `workflows-to-install/` directory after copying the files to `.github/workflows/`.
