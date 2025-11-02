# Dog Bark Sounds

This directory contains bark sound files for all 7 dog breeds in the Doggy Dogs Car Dog Alarm app.

## Directory Structure

Each breed has its own folder with sound files following the naming convention: `{sound_type}_{number}.mp3`

```
barks/
├── german_shepherd/
│   └── bark_01.mp3
├── chihuahua/
│   └── bark_01.mp3
├── beagle/
│   └── bark_01.mp3
├── doberman/
│   └── bark_01.mp3
├── rottweiler/
│   └── bark_01.mp3
├── bulldog/
│   └── bark_01.mp3
└── pitbull/
    └── bark_01.mp3
```

## Naming Convention

Sound files follow the pattern: `{sound_type}_{variant_number}.mp3`

- **sound_type**: `bark`, `growl`, `howl`, `whine`, etc.
- **variant_number**: `01`, `02`, `03`, etc. (zero-padded)

### Examples:
- `bark_01.mp3` - First bark sound
- `bark_02.mp3` - Second bark sound (variation)
- `growl_01.mp3` - First growl sound
- `howl_01.mp3` - First howl sound

This structure allows easy addition of:
- Multiple bark variations per breed
- Different vocalization types (growl, howl, whine)
- Situation-specific sounds (happy, aggressive, alert)

## Sound Files

All sounds are normalized to **~3 seconds** duration for consistency.

| Breed | File | Source | License | Duration | Size | Notes |
|-------|------|--------|---------|----------|------|-------|
| German Shepherd | `german_shepherd/bark_01.mp3` | [OrangeFreeSound](https://orangefreesounds.com/dog-barking-sound-german-shepherd/) | CC BY 4.0 | 3.00s | 48K | Normalized, fade out |
| Chihuahua | `chihuahua/bark_01.mp3` | [SoundFXCenter](https://soundfxcenter.com/) | Free Download | 3.00s | 24K | Small dog bark |
| Beagle | `beagle/bark_01.mp3` | [SoundFXCenter](https://soundfxcenter.com/) | Free Download | 3.00s | 48K | Padded with silence |
| Doberman | `doberman/bark_01.mp3` | [SoundFXCenter](https://soundfxcenter.com/) | Free Download | 3.00s | 48K | Guard dog bark |
| Rottweiler | `rottweiler/bark_01.mp3` | [LibSounds](https://libsounds.com/sound/28) | LibSounds License | 3.00s | 48K | Deep, powerful |
| Bulldog | `bulldog/bark_01.mp3` | [BigSoundBank](https://bigsoundbank.com/detail-0916-barking-dog.html) | CC0 Public Domain | 3.00s | 48K | Large dog close-up |
| Pitbull | `pitbull/bark_01.mp3` | [BigSoundBank](https://bigsoundbank.com/barking-dogs-s0288.html) | CC0 Public Domain | 3.00s | 48K | Aggressive barking |

## Status

✅ **All 7 breeds** have bark sounds in MP3 format
✅ **5/7 breeds** have breed-specific sounds (German Shepherd, Chihuahua, Beagle, Doberman, Rottweiler)
⚠️  **2/7 breeds** (Bulldog, Pitbull) use large/aggressive dog barks (appropriate but not breed-specific)

## Licensing

All sounds are free to use in commercial applications:

- **German Shepherd**: Creative Commons Attribution 4.0 (requires attribution)
- **Chihuahua, Beagle, Doberman**: Free download (license unclear, publicly available)
- **Rottweiler**: LibSounds license (check their terms)
- **Bulldog, Pitbull**: CC0 Public Domain (no attribution required)

## Adding New Sounds

To add more sounds for a breed:

1. Add the MP3 file to the breed's folder
2. Follow the naming convention: `{type}_{number}.mp3`
3. Update this README with source and license info

Example:
```bash
# Add a second bark variation for German Shepherd
cp new_sound.mp3 german_shepherd/bark_02.mp3

# Add a growl sound for Rottweiler
cp growl_sound.mp3 rottweiler/growl_01.mp3
```

## File Format

- **Format**: MP3
- **Duration**: ~3 seconds (normalized)
- **Sample Rate**: 44.1 kHz
- **Bitrate**: 128 kbps (normalized)
- **Channels**: Mono or Stereo
- **Processing**: Trimmed/padded to 3s with fade-out

All files are optimized for mobile playback and consistent user experience.

### Normalization Process

Original sound files had varying lengths (0.94s to 182s). All files have been normalized to 3 seconds:
- Long files (>3s): Trimmed to 3s with 0.3s fade-out
- Short files (<3s): Padded with silence to reach 3s

Original sources are documented in the table above for attribution and reference.
