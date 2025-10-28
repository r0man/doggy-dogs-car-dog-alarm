# iOS Code Signing Setup Guide

Complete guide for setting up iOS app signing with Fastlane Match and GitHub Actions.

## Table of Contents
- [Overview](#overview)
- [Prerequisites](#prerequisites)
- [Initial Setup](#initial-setup)
- [Fastlane Match Setup](#fastlane-match-setup)
- [GitHub Secrets Configuration](#github-secrets-configuration)
- [Local Testing](#local-testing)
- [TestFlight Deployment](#testflight-deployment)
- [Troubleshooting](#troubleshooting)

---

## Overview

iOS code signing is required to distribute apps. This guide uses **Fastlane Match** to manage certificates and provisioning profiles.

**Why Fastlane Match?**
- ✅ Centralized certificate management
- ✅ Team synchronization (everyone uses same certificates)
- ✅ Git-based storage with encryption
- ✅ Automatic certificate renewal
- ✅ Works seamlessly with CI/CD

---

## Prerequisites

- **Apple Developer Account** ($99/year)
- **App ID** registered in Apple Developer Portal
- **Private Git repository** for storing certificates (separate from your code repo)
- **macOS** for local testing
- **Fastlane** installed: `gem install fastlane`

---

## Initial Setup

### 1. Create Certificates Repository

Create a **private** GitHub repository for storing certificates:

```bash
# Example: https://github.com/your-org/doggy-dogs-certificates
# IMPORTANT: This repo must be private and separate from your app repo
```

### 2. Generate SSH Deploy Key

Create an SSH key for GitHub Actions to access the certificates repo:

```bash
# Generate SSH key (no passphrase)
ssh-keygen -t ed25519 -C "github-actions@doggy-dogs" -f ~/.ssh/match_deploy_key -N ""

# Display public key (add to certificates repo as Deploy Key with write access)
cat ~/.ssh/match_deploy_key.pub

# Display private key (save as GitHub Secret: MATCH_GIT_PRIVATE_KEY)
cat ~/.ssh/match_deploy_key
```

**In your certificates repo:**
- Go to Settings → Deploy keys → Add deploy key
- Paste the **public key**
- ✅ Check "Allow write access"

### 3. Install Fastlane (macOS)

```bash
# Install Fastlane
sudo gem install fastlane

# Navigate to iOS folder
cd ios

# Initialize Fastlane
fastlane init

# Select option 4: Manual setup
```

---

## Fastlane Match Setup

### 1. Create Matchfile

In your `ios/` directory, create `Matchfile`:

```ruby
git_url("git@github.com:your-org/doggy-dogs-certificates.git")
storage_mode("git")
type("appstore")
app_identifier(["com.example.doggyDogsCarDogAlarm"])
username("your-apple-id@example.com")
```

### 2. Create Fastfile

In `ios/fastlane/Fastfile`:

```ruby
default_platform(:ios)

platform :ios do
  desc "Sync code signing"
  lane :sync_signing do
    match(
      type: "appstore",
      readonly: false
    )
  end

  desc "Build IPA"
  lane :build do
    build_app(
      scheme: "Runner",
      export_method: "app-store",
      export_options: {
        provisioningProfiles: {
          "com.example.doggyDogsCarDogAlarm" => "match AppStore com.example.doggyDogsCarDogAlarm"
        }
      }
    )
  end

  desc "Upload to TestFlight"
  lane :beta do
    sync_signing
    build
    upload_to_testflight(
      skip_waiting_for_build_processing: true
    )
  end
end
```

### 3. Run Match for First Time

```bash
cd ios

# This will create certificates and upload to git repo
# You'll be prompted to create a passphrase - SAVE THIS!
fastlane match appstore

# Enter your Apple ID and app-specific password when prompted
```

**Important:** The passphrase you create will be stored as `MATCH_PASSWORD` in GitHub Secrets.

### 4. Create ExportOptions.plist

In `ios/ExportOptions.plist`:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>method</key>
    <string>app-store</string>
    <key>teamID</key>
    <string>YOUR_TEAM_ID</string>
    <key>uploadBitcode</key>
    <false/>
    <key>compileBitcode</key>
    <false/>
    <key>uploadSymbols</key>
    <true/>
    <key>signingStyle</key>
    <string>manual</string>
    <key>provisioningProfiles</key>
    <dict>
        <key>com.example.doggyDogsCarDogAlarm</key>
        <string>match AppStore com.example.doggyDogsCarDogAlarm</string>
    </dict>
</dict>
</plist>
```

Find your Team ID:
```bash
# Option 1: Fastlane
fastlane fastlane-credentials show

# Option 2: Apple Developer Portal
# https://developer.apple.com/account → Membership → Team ID
```

---

## GitHub Secrets Configuration

Add these secrets to your GitHub repository (Settings → Secrets and variables → Actions → New repository secret):

### Required Secrets

| Secret Name | Description | How to Get |
|-------------|-------------|------------|
| `MATCH_GIT_URL` | SSH URL of certificates repo | `git@github.com:your-org/doggy-dogs-certificates.git` |
| `MATCH_GIT_PRIVATE_KEY` | Private SSH key | Contents of `~/.ssh/match_deploy_key` |
| `MATCH_PASSWORD` | Encryption passphrase | Password you set when running `fastlane match` |
| `FASTLANE_USER` | Apple ID email | Your Apple Developer account email |
| `FASTLANE_APPLE_APPLICATION_SPECIFIC_PASSWORD` | App-specific password | See below |

### Generate Apple App-Specific Password

1. Go to https://appleid.apple.com/account/manage
2. Sign in with your Apple ID
3. Under "Security" → "App-Specific Passwords" → Click "+"
4. Enter label: "GitHub Actions CI/CD"
5. **Copy the password** (you can't view it again!)
6. Save as `FASTLANE_APPLE_APPLICATION_SPECIFIC_PASSWORD`

### Example Secret Values

```bash
# MATCH_GIT_URL
git@github.com:myorg/doggy-certificates.git

# MATCH_GIT_PRIVATE_KEY (entire private key including headers)
-----BEGIN OPENSSH PRIVATE KEY-----
b3BlbnNzaC1rZXktdjEAAAAABG5vbmUAAAAEbm9uZQAAAAAAAAABAAAAMwAAAAtzc2gtZW
...
-----END OPENSSH PRIVATE KEY-----

# MATCH_PASSWORD
MySecureP@ssw0rd!2025

# FASTLANE_USER
developer@example.com

# FASTLANE_APPLE_APPLICATION_SPECIFIC_PASSWORD
abcd-efgh-ijkl-mnop
```

---

## Local Testing

Before pushing to GitHub Actions, test locally:

```bash
cd ios

# 1. Test Match sync (should download certificates)
fastlane match appstore --readonly

# 2. Test build
cd ..
flutter build ipa --release --export-options-plist=ios/ExportOptions.plist

# 3. Verify IPA was created
ls -lh build/ios/ipa/

# 4. Test Fastlane build (optional)
cd ios
fastlane build
```

If successful, you should see:
```
✅ Successfully exported and signed IPA
📦 IPA location: build/ios/ipa/doggy_dogs_car_dog_alarm.ipa
```

---

## TestFlight Deployment

### Automatic Upload (in workflow)

The workflow automatically uploads to TestFlight when:
- Pushing to `main` branch
- All secrets are configured
- Build is successful

### Manual Upload

```bash
cd ios
fastlane beta
```

### TestFlight App Information

Before first upload, configure in App Store Connect:
1. Go to https://appstoreconnect.apple.com
2. My Apps → Your App → TestFlight
3. Fill in: Export Compliance, Beta App Description, Feedback Email
4. Add internal testers

---

## Troubleshooting

### Common Issues

#### 1. "No code signing identity found"

**Cause:** Certificates not synced properly

**Solution:**
```bash
cd ios
fastlane match appstore --readonly
```

#### 2. "Provisioning profile doesn't include signing certificate"

**Cause:** Certificate/profile mismatch

**Solution:**
```bash
# Regenerate certificates (removes old ones)
cd ios
fastlane match nuke appstore
fastlane match appstore
```

⚠️ **Warning:** `nuke` deletes all certificates. Only use if you're sure.

#### 3. SSH authentication failed in GitHub Actions

**Cause:** SSH key not configured correctly

**Solution:**
- Verify `MATCH_GIT_PRIVATE_KEY` includes `-----BEGIN` and `-----END` lines
- Ensure deploy key has write access in certificates repo
- Check SSH URL format: `git@github.com:org/repo.git` (not HTTPS)

#### 4. "Could not find a matching code signing identity"

**Cause:** ExportOptions.plist misconfigured

**Solution:**
- Verify Team ID is correct in `ExportOptions.plist`
- Ensure provisioning profile name matches Match output
- Check bundle identifier matches

#### 5. TestFlight upload fails

**Cause:** App-specific password invalid or expired

**Solution:**
- Generate new app-specific password
- Update `FASTLANE_APPLE_APPLICATION_SPECIFIC_PASSWORD` secret
- Verify `FASTLANE_USER` is correct Apple ID

#### 6. "Match failed to decrypt the repo"

**Cause:** Wrong `MATCH_PASSWORD`

**Solution:**
- Verify the passphrase you used during initial `fastlane match` setup
- Update `MATCH_PASSWORD` secret in GitHub

### Debug Commands

```bash
# View Match status
fastlane match appstore --readonly --verbose

# List certificates
security find-identity -v -p codesigning

# Verify IPA signature
codesign -dv --verbose=4 build/ios/ipa/doggy_dogs_car_dog_alarm.ipa

# Check provisioning profile
security cms -D -i ~/Library/MobileDevice/Provisioning\ Profiles/*.mobileprovision
```

---

## Security Best Practices

### 🔒 Secrets Management

1. **Never commit secrets** to your code repository
2. **Rotate passwords** every 90 days
3. **Use separate certificates repo** (private)
4. **Limit access** to certificates repo (team members only)
5. **Enable 2FA** on Apple ID
6. **Use app-specific passwords** (never main Apple ID password)

### 🔐 Certificate Storage

```bash
# Certificates repo should have:
- Private repository visibility
- Limited collaborators
- Deploy key with write access (for CI)
- Regular key rotation (annually)
```

### 📱 TestFlight

- Enable **Encryption Export Compliance** in App Store Connect
- Add **Beta Tester Agreement** if collecting data
- Configure **Feedback Email** for tester communication

---

## Quick Reference

### First-Time Setup Checklist

- [ ] Create private certificates Git repo
- [ ] Generate SSH deploy key
- [ ] Add deploy key to certificates repo (with write access)
- [ ] Install Fastlane: `gem install fastlane`
- [ ] Create `Matchfile` in `ios/`
- [ ] Run `fastlane match appstore` (save passphrase!)
- [ ] Create `ExportOptions.plist` with Team ID
- [ ] Generate Apple app-specific password
- [ ] Add all 5 secrets to GitHub repository
- [ ] Test local build: `flutter build ipa`
- [ ] Push to GitHub and verify workflow runs

### Required Files

```
ios/
├── Matchfile                    # Match configuration
├── ExportOptions.plist          # Export settings
└── fastlane/
    └── Fastfile                 # Fastlane lanes
```

### Workflow Triggers

| Branch | Event | Output |
|--------|-------|--------|
| Any | Pull Request | Debug build (unsigned) |
| main | Push | Signed IPA + TestFlight upload |
| develop | Push | Signed IPA (no TestFlight) |

---

## Additional Resources

- [Fastlane Match Docs](https://docs.fastlane.tools/actions/match/)
- [Flutter iOS Deployment](https://docs.flutter.dev/deployment/ios)
- [Apple Developer Portal](https://developer.apple.com/account/)
- [App Store Connect](https://appstoreconnect.apple.com/)
- [Code Signing Guide](https://codesigning.guide/)

---

**Need Help?**

If you encounter issues not covered here:
1. Check [Fastlane Discussions](https://github.com/fastlane/fastlane/discussions)
2. Review [GitHub Actions iOS Setup](https://docs.github.com/en/actions/deployment/deploying-xcode-applications/installing-an-apple-certificate-on-macos-runners-for-xcode-development)
3. Search [Stack Overflow: fastlane-match](https://stackoverflow.com/questions/tagged/fastlane-match)
