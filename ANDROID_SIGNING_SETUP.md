# Android Signing Setup for CI/CD

This guide explains how to set up Android app signing for automated builds with GitHub Actions.

---

## Overview

To publish Android apps to the Play Store or distribute signed APKs, you need to sign your app with a keystore. This document covers:

1. Generating a keystore
2. Configuring Android build.gradle
3. Setting up GitHub Secrets
4. Testing the setup

---

## Step 1: Generate Release Keystore

### Option A: Using keytool (Recommended)

```bash
keytool -genkey -v -keystore release.keystore \
  -alias release \
  -keyalg RSA \
  -keysize 2048 \
  -validity 10000

# You'll be prompted for:
# - Keystore password (remember this!)
# - Key password (remember this!)
# - Your name, organization, city, state, country
```

### Option B: Using Android Studio

1. Open your project in Android Studio
2. Go to **Build → Generate Signed Bundle / APK**
3. Click **Create new...**
4. Fill in the form and click **OK**
5. Save the keystore file as `release.keystore`

### Important Notes

⚠️ **NEVER commit your keystore to Git!**

- Store the keystore file securely (password manager, encrypted backup)
- Keep a backup in a safe location
- If you lose the keystore, you cannot update your app on Play Store
- Document the passwords in a secure location

---

## Step 2: Configure Android Build

### 2.1 Update android/app/build.gradle

Add the following code to `android/app/build.gradle`:

```gradle
// Add this at the top of the file, after 'apply plugin' statements
def keystoreProperties = new Properties()
def keystorePropertiesFile = rootProject.file('key.properties')
if (keystorePropertiesFile.exists()) {
    keystoreProperties.load(new FileInputStream(keystorePropertiesFile))
}

android {
    ...

    // Add signing configs
    signingConfigs {
        release {
            if (keystorePropertiesFile.exists()) {
                keyAlias keystoreProperties['keyAlias']
                keyPassword keystoreProperties['keyPassword']
                storeFile keystoreProperties['storeFile'] ? file(keystoreProperties['storeFile']) : null
                storePassword keystoreProperties['storePassword']
            }
        }
    }

    buildTypes {
        release {
            // Use the signing config
            signingConfig signingConfigs.release

            // Existing release settings...
            minifyEnabled true
            shrinkResources true
        }
    }
}
```

### 2.2 Create .gitignore entries

Add to `android/.gitignore`:

```
# Signing files
*.jks
*.keystore
key.properties
```

Add to root `.gitignore` (if not already present):

```
# Android signing
android/key.properties
android/app/*.jks
android/app/*.keystore
```

---

## Step 3: Set Up GitHub Secrets

### 3.1 Encode the Keystore

Convert your keystore to base64:

```bash
# On Linux/Mac:
openssl base64 < release.keystore | tr -d '\n' > keystore.base64.txt

# On Windows (PowerShell):
[Convert]::ToBase64String([IO.File]::ReadAllBytes("release.keystore")) > keystore.base64.txt
```

### 3.2 Add Secrets to GitHub

1. Go to your repository on GitHub
2. Navigate to **Settings → Secrets and variables → Actions**
3. Click **New repository secret**
4. Add the following secrets:

| Secret Name | Value | Example |
|-------------|-------|---------|
| `ANDROID_KEYSTORE_BASE64` | Contents of `keystore.base64.txt` | `MIIKdQIBAzCCCj8GCS...` |
| `KEYSTORE_PASSWORD` | Your keystore password | `MySecurePassword123` |
| `KEY_ALIAS` | Your key alias | `release` |
| `KEY_PASSWORD` | Your key password | `MyKeyPassword456` |

### 3.3 Verify Secrets

- Don't include quotes around values
- Don't include line breaks in base64 string
- Test each password is correct
- Key alias must match exactly (case-sensitive)

---

## Step 4: Local Testing (Optional but Recommended)

### 4.1 Create local key.properties

Create `android/key.properties` (this file is gitignored):

```properties
storePassword=YourKeystorePassword
keyPassword=YourKeyPassword
keyAlias=release
storeFile=keystore.jks
```

### 4.2 Copy keystore to android/app/

```bash
cp release.keystore android/app/keystore.jks
```

### 4.3 Build locally

```bash
# Build release APK
flutter build apk --release

# Build App Bundle
flutter build appbundle --release
```

### 4.4 Verify signing

```bash
# Check APK signature
keytool -printcert -jarfile build/app/outputs/flutter-apk/app-release.apk

# Check AAB signature
jarsigner -verify -verbose -certs build/app/outputs/bundle/release/app-release.aab
```

You should see your certificate details and "jar verified."

---

## Step 5: Test GitHub Actions

### 5.1 Push to trigger workflow

```bash
git push origin main
```

### 5.2 Monitor the workflow

1. Go to **Actions** tab on GitHub
2. Click on the latest workflow run
3. Check **Build Android APK** job
4. Look for these success messages:
   - "✅ Keystore decoded successfully"
   - "✅ key.properties created"
   - "✅ Release APK built successfully"

### 5.3 Download and verify artifact

1. Scroll to bottom of workflow run page
2. Download **android-apk-{version}** artifact
3. Extract and verify the APK:

```bash
unzip android-apk-1.0.0+1.zip
keytool -printcert -jarfile app-release.apk
```

---

## Troubleshooting

### "Keystore not found"

- Verify `ANDROID_KEYSTORE_BASE64` secret is set
- Check for extra whitespace or line breaks
- Re-encode the keystore and update secret

### "Invalid keystore format"

- Keystore might be corrupted during encoding
- Try encoding again: `openssl base64 -A < release.keystore`
- Verify the keystore locally first

### "Incorrect password"

- Double-check all passwords are correct
- Passwords are case-sensitive
- No quotes should be in secrets

### "Key alias not found"

- Verify alias matches exactly
- List aliases: `keytool -list -keystore release.keystore`
- Update `KEY_ALIAS` secret if needed

### "Signing config not found"

- Check `key.properties` file is created in workflow
- Verify `build.gradle` has signing configuration
- Check for syntax errors in build.gradle

### Debugging locally

Create test script:

```bash
#!/bin/bash
# test-signing.sh

echo "$ANDROID_KEYSTORE_BASE64" | base64 --decode > test-keystore.jks

if [ -f "test-keystore.jks" ]; then
    echo "✅ Keystore decoded"
    keytool -list -keystore test-keystore.jks -storepass "$KEYSTORE_PASSWORD"
else
    echo "❌ Failed to decode keystore"
fi

rm -f test-keystore.jks
```

Run with your secrets:

```bash
export ANDROID_KEYSTORE_BASE64="paste-from-secret"
export KEYSTORE_PASSWORD="your-password"
./test-signing.sh
```

---

## Security Best Practices

### Do's ✅

- **Keep keystore in secure location** (password manager, encrypted backup)
- **Document passwords securely** (not in code or plain text files)
- **Use strong passwords** (minimum 16 characters, mixed case, numbers, symbols)
- **Limit access** to keystore and passwords
- **Rotate secrets** every 90 days if possible
- **Monitor access** to GitHub Secrets
- **Use separate keystores** for debug vs release

### Don'ts ❌

- **Never commit keystore** to Git (even private repos)
- **Never share keystore** publicly
- **Never reuse passwords** from other services
- **Never store passwords** in code comments
- **Never skip backups** (you can't recover a lost keystore)
- **Never use weak passwords** like "password123"

---

## Advanced: Multiple Build Variants

If you have multiple app variants (dev, staging, prod):

```gradle
android {
    signingConfigs {
        debug {
            storeFile file("debug.keystore")
            storePassword "android"
            keyAlias "androiddebugkey"
            keyPassword "android"
        }

        staging {
            // Use same release keystore but different alias
            keyAlias keystoreProperties['stagingKeyAlias']
            keyPassword keystoreProperties['stagingKeyPassword']
            storeFile file(keystoreProperties['storeFile'])
            storePassword keystoreProperties['storePassword']
        }

        release {
            keyAlias keystoreProperties['keyAlias']
            keyPassword keystoreProperties['keyPassword']
            storeFile file(keystoreProperties['storeFile'])
            storePassword keystoreProperties['storePassword']
        }
    }

    buildTypes {
        debug {
            signingConfig signingConfigs.debug
        }

        staging {
            signingConfig signingConfigs.staging
        }

        release {
            signingConfig signingConfigs.release
        }
    }
}
```

---

## Play Store Publishing

### Preparing for Play Store

1. **Build App Bundle (AAB)** - preferred format:
   ```bash
   flutter build appbundle --release
   ```

2. **Upload to Play Console**:
   - Go to [Google Play Console](https://play.google.com/console)
   - Create app (if first time)
   - Navigate to Release → Production → Create new release
   - Upload AAB file

3. **App Signing by Google** (Recommended):
   - Let Google manage your app signing key
   - You only manage the upload key
   - More secure and allows key upgrade if compromised

### Automated Play Store Deployment (Future)

Consider these tools for automation:
- **Fastlane** - Full automation suite
- **Gradle Play Publisher** - Plugin for Play Store uploads
- **GitHub Action** - `r0adkll/upload-google-play`

---

## Checklist

Before committing to main:

- [ ] Keystore generated and backed up securely
- [ ] Passwords documented in secure location
- [ ] `build.gradle` configured with signing config
- [ ] `.gitignore` updated to exclude keystore files
- [ ] GitHub Secrets configured (4 secrets)
- [ ] Tested local build with signing
- [ ] Verified signature on built APK
- [ ] Tested GitHub Actions workflow
- [ ] Downloaded and verified CI-built APK

---

## Quick Reference

### Generate Keystore
```bash
keytool -genkey -v -keystore release.keystore -alias release \
  -keyalg RSA -keysize 2048 -validity 10000
```

### Encode Keystore
```bash
openssl base64 < release.keystore | tr -d '\n' > keystore.base64.txt
```

### Verify APK Signature
```bash
keytool -printcert -jarfile app-release.apk
```

### Build Release APK
```bash
flutter build apk --release
```

### Build App Bundle
```bash
flutter build appbundle --release
```

---

## Resources

- [Flutter Deployment Docs](https://docs.flutter.dev/deployment/android)
- [Android App Signing](https://developer.android.com/studio/publish/app-signing)
- [Play Console Help](https://support.google.com/googleplay/android-developer)
- [GitHub Actions Secrets](https://docs.github.com/en/actions/security-guides/encrypted-secrets)

---

## Support

If you encounter issues:

1. Check the **Troubleshooting** section above
2. Review workflow logs in GitHub Actions
3. Verify all secrets are set correctly
4. Test signing locally first
5. Check Flutter and Android versions match CI

---

**Last Updated**: 2025-10-28
**Next Review**: Before first Play Store release
