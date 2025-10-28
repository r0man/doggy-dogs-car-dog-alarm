# Bark Sound Assets

This directory contains the bark sound files for the Doggy Dogs Car Alarm app.

## Directory Structure

Each dog breed has its own subdirectory with bark sound files:

```
assets/sounds/
├── germanshepherd/
├── rottweiler/
├── doberman/
├── bulldog/
├── pitbull/
├── husky/
└── beagle/
```

## File Naming Convention

Files follow the pattern: `{barkType}_{intensity}.mp3`

### Bark Types:
- `warning` - Single warning bark (1 second)
- `alert` - Alert barking sequence (3 seconds)
- `aggressive` - Aggressive barking (5 seconds)
- `threat` - Intense threat barking (8 seconds)

### Intensity Levels:
- `low` - Quiet, less threatening
- `medium` - Normal barking volume
- `high` - Loud, intimidating
- `maximum` - Maximum volume and aggression

## Required Files (Per Breed)

Each breed directory needs 16 audio files:

1. `warning_low.mp3`
2. `warning_medium.mp3`
3. `warning_high.mp3`
4. `warning_maximum.mp3`
5. `alert_low.mp3`
6. `alert_medium.mp3`
7. `alert_high.mp3`
8. `alert_maximum.mp3`
9. `aggressive_low.mp3`
10. `aggressive_medium.mp3`
11. `aggressive_high.mp3`
12. `aggressive_maximum.mp3`
13. `threat_low.mp3`
14. `threat_medium.mp3`
15. `threat_high.mp3`
16. `threat_maximum.mp3`

Total: **112 audio files** (7 breeds × 16 files each)

## Audio Specifications

- **Format**: MP3
- **Sample Rate**: 44.1 kHz
- **Bit Rate**: 128 kbps or higher
- **Channels**: Mono or Stereo
- **Duration**:
  - Warning: ~1 second
  - Alert: ~3 seconds
  - Aggressive: ~5 seconds
  - Threat: ~8 seconds

## Sound Characteristics by Breed

### German Shepherd
- Deep, authoritative bark
- Clear and commanding
- Natural guard dog sound

### Rottweiler
- Very deep, rumbling bark
- Powerful and intimidating
- Low frequency emphasis

### Doberman
- Sharp, alert bark
- Medium-high pitch
- Quick, staccato pattern

### Bulldog
- Gruff, raspy bark
- Medium pitch
- Distinctive gravelly quality

### Pitbull
- Strong, confident bark
- Medium pitch
- Persistent and determined

### Husky
- Vocal, howl-like quality
- Can be higher pitched
- Talkative, expressive

### Beagle
- Higher pitched bark
- Persistent "bay" quality
- More rapid fire

## Sourcing Audio

### Options for obtaining bark sounds:

1. **Professional Sound Libraries**:
   - freesound.org (Creative Commons)
   - AudioJungle
   - Epidemic Sound
   - SoundDogs

2. **Recording Real Dogs**:
   - Work with dog trainers
   - Record at dog parks (with permission)
   - Hire professional dog handlers

3. **AI-Generated Sounds**:
   - Use AI audio generation tools
   - Modify and enhance with audio editing

4. **Placeholder Sounds**:
   - Currently, the app will continue silently if files are missing
   - In development, you can use generic barks for all breeds

## Audio Processing Tips

1. **Normalize Volume**: Ensure consistent peak levels
2. **Remove Background Noise**: Clean up recordings
3. **Add Reverb** (optional): For more intimidating effect
4. **Fade In/Out**: Smooth transitions at start/end
5. **Loop Preparation**: If creating loops, ensure seamless transitions

## Testing

Test each sound file:
```bash
flutter run
# Navigate to Alarm Screen
# Activate alarm with different modes
# Verify barks play correctly
```

## Legal Considerations

- Ensure you have proper licensing for all audio files
- Attribute sources as required by licenses
- Consider volume limits based on local regulations
- Add user controls for maximum volume

## Future Enhancements

- [ ] Dynamic pitch shifting based on dog level
- [ ] Layered sounds for more realistic barking
- [ ] Environmental reverb effects
- [ ] Breed-mixing for adopted dogs
- [ ] User-uploaded custom barks
