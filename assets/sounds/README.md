# Dog Bark Sounds

This directory contains bark sound files for all 8 dog breeds in the Doggy Dogs Car Dog Alarm app.

## Directory Structure

Each breed has its own folder with sound files following the code's expected naming convention:

```
assets/sounds/
├── germanshepherd/
│   ├── bark_01.mp3 (master file)
│   ├── warning_low.mp3 -> bark_01.mp3
│   ├── warning_medium.mp3 -> bark_01.mp3
│   ├── ... (16 variants total)
├── rottweiler/
├── doberman/
├── bulldog/
├── pitbull/
├── husky/
├── beagle/
└── chihuahua/
```

## Current Implementation

**Status**: ✅ All 8 breeds have bark sounds implemented

Each breed folder contains:
- **1 master file**: `bark_01.mp3` (normalized to 3 seconds, ~24-48KB)
- **16 symbolic links**: All bark type/intensity combinations pointing to `bark_01.mp3`

This allows the app to work immediately while we can later add authentic variations for different types and intensities.

## File Naming Convention

The code expects files in the pattern: `{type}_{intensity}.mp3`

### Bark Types:
- `warning` - Single warning bark
- `alert` - Alert barking sequence
- `aggressive` - Aggressive barking
- `threat` - Intense threat barking

### Intensity Levels:
- `low` - Quieter volume (40%)
- `medium` - Normal volume (60%)
- `high` - Loud volume (80%)
- `maximum` - Maximum volume (100%)

### Example Files:
- `warning_low.mp3` - Quiet warning bark
- `alert_medium.mp3` - Normal alert bark
- `aggressive_high.mp3` - Loud aggressive bark
- `threat_maximum.mp3` - Maximum threat bark

**Total per breed**: 16 files (4 types × 4 intensities)
**Total in app**: 128 files (8 breeds × 16 files)

## Sound Files - Sources & Licensing

All sounds are normalized to **3 seconds** duration for consistency.

| Breed | Master File | Source | License | Duration | Size | Notes |
|-------|-------------|--------|---------|----------|------|-------|
| German Shepherd | `germanshepherd/bark_01.mp3` | [OrangeFreeSound](https://orangefreesounds.com/dog-barking-sound-german-shepherd/) | CC BY 4.0 | 3.00s | 48K | Normalized, fade out |
| Chihuahua | `chihuahua/bark_01.mp3` | [SoundFXCenter](https://soundfxcenter.com/) | Free to use* | 3.00s | 24K | Small dog bark |
| Beagle | `beagle/bark_01.mp3` | [SoundFXCenter](https://soundfxcenter.com/) | Free to use* | 3.00s | 48K | Padded with silence |
| Doberman | `doberman/bark_01.mp3` | [SoundFXCenter](https://soundfxcenter.com/) | Free to use* | 3.00s | 48K | Guard dog bark |
| Rottweiler | `rottweiler/bark_01.mp3` | [LibSounds](https://libsounds.com/sound/28) | LibSounds License** | 3.00s | 48K | Deep, powerful |
| Bulldog | `bulldog/bark_01.mp3` | [BigSoundBank](https://bigsoundbank.com/detail-0916-barking-dog.html) | CC0 Public Domain | 3.00s | 48K | Large dog close-up |
| Pitbull | `pitbull/bark_01.mp3` | [BigSoundBank](https://bigsoundbank.com/barking-dogs-s0288.html) | CC0 Public Domain | 3.00s | 48K | Aggressive barking |
| Husky | `husky/bark_01.mp3` | [BigSoundBank](https://bigsoundbank.com/detail-0916-barking-dog.html) | CC0 Public Domain | 3.00s | 48K | Large dog bark |

### Licensing Notes:

- **CC BY 4.0** (German Shepherd): Requires attribution - "Dog barking sound by OrangeFreeSound"
- **CC0 Public Domain** (Bulldog, Pitbull, Husky): No attribution required, free for commercial use
- ***SoundFXCenter** (Chihuahua, Beagle, Doberman): Publicly available for free download. License terms state "free to use" but should verify commercial terms before distribution.
- ****LibSounds** (Rottweiler): Check LibSounds.com terms of service for commercial use rights.

**TODO**: Clarify licensing for SoundFXCenter and LibSounds files before commercial distribution.

## Audio Format

- **Format**: MP3
- **Duration**: 3 seconds (normalized)
- **Sample Rate**: 44.1-48 kHz
- **Bitrate**: 128 kbps
- **Channels**: Mono or Stereo
- **Processing**: Trimmed/padded to 3s with fade-out

All files are optimized for mobile playback and consistent user experience.

## Future Enhancements

### Phase 1: Type Variations (Priority)
Add authentic variations for bark types:
- [ ] Short warning bark (~1s)
- [ ] Alert bark sequence (~3s)
- [ ] Aggressive barking (~5s)
- [ ] Intense threat bark (~8s)

### Phase 2: Intensity Variations
Record or synthesize intensity levels:
- [ ] Volume modulation
- [ ] Frequency filtering
- [ ] Reverb/echo effects

### Phase 3: Advanced Features
- [ ] Dynamic pitch shifting based on dog level
- [ ] Layered sounds for realism
- [ ] Environmental reverb
- [ ] Breed-specific vocalizations (growls, howls)

## Adding New Sounds

To replace a breed's bark sound:

1. Add the MP3 file to the breed's folder as `bark_01.mp3`
2. Normalize to 3 seconds:
   ```bash
   ffmpeg -i input.mp3 -t 3.0 -af "afade=t=out:st=2.7:d=0.3" bark_01.mp3 -y
   ```
3. The symbolic links will automatically use the new file
4. Update this README with source and license info

To add type/intensity-specific variations:

1. Replace the symbolic link with an actual file:
   ```bash
   cd germanshepherd
   rm warning_low.mp3
   cp my_quiet_warning.mp3 warning_low.mp3
   ```
2. Ensure correct duration for the type (warning=1s, alert=3s, etc.)

## Testing

Verify sounds load and play correctly:

```bash
# Run tests
flutter test

# Test on device
flutter run
# Navigate to Alarm Screen → Settings → Test Bark Sounds
# Verify all breeds play correctly
```

## Attribution

When distributing the app, include in credits/about screen:

> German Shepherd bark sound by [OrangeFreeSound](https://orangefreesounds.com/) licensed under CC BY 4.0
>
> Additional dog bark sounds from BigSoundBank (CC0), SoundFXCenter, and LibSounds
