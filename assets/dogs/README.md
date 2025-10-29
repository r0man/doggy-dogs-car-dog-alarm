# Dog Sprite Assets - Manga + Fritz the Cat Hybrid Style

## Overview
This directory contains all dog breed sprite assets blending **friendly Japanese manga aesthetic** with **Fritz the Cat's streetwise personality**. Each breed has multiple animation states to create an engaging, personality-rich experience.

## Style Guide
See `MANGA_FRITZ_STYLE_GUIDE.md` for comprehensive visual design specifications.

**Style Philosophy**: "Friendly street-smart" - Big manga eyes meet urban cool. These are approachable manga dogs with attitude, confidence, and personality.

## File Structure

### Naming Convention
```
{breed}_{animation_state}.svg
```

### Breeds (7 total)
1. `german_shepherd` - Seasoned veteran cop-dog
2. `rottweiler` - Tough bouncer
3. `doberman` - Sleek street racer
4. `bulldog` - Grizzled bartender
5. `pitbull` - Misunderstood loyal muscle
6. `husky` - Sarcastic comedian
7. `beagle` - Scrappy street kid

### Animation States (8 total)
1. `idle` - Default casual stance (PRIMARY)
2. `alert` - Vigilant, ears up, ready
3. `barking` - Active alarm, aggressive
4. `happy` - Joyful, tongue out, tail wagging
5. `sad` - Low energy, droopy
6. `sleeping` - Resting, eyes closed
7. `eating` - Head down, munching
8. `playing` - Bouncy, energetic

### Complete Sprite Set (56 files total)
Each breed needs 8 animation states:
- german_shepherd_idle.svg ✅
- german_shepherd_alert.svg ⏳
- german_shepherd_barking.svg ✅
- german_shepherd_happy.svg ✅
- german_shepherd_sad.svg ⏳
- german_shepherd_sleeping.svg ⏳
- german_shepherd_eating.svg ⏳
- german_shepherd_playing.svg ⏳

...and 6 more breeds x 8 states each.

## Current Status

### Completed
- ✅ Style Guide (`MANGA_FRITZ_STYLE_GUIDE.md` v2.0 - Hybrid approach)
- ✅ German Shepherd idle state (REFERENCE IMPLEMENTATION - Manga + Fritz)
- ✅ German Shepherd barking state (Manga + Fritz)
- ✅ German Shepherd happy state (Manga + Fritz)

**Style Features:**
- Big manga eyes with Fritz hooded/knowing looks
- Bold outlines (3-5px) added to manga shapes
- Expressive eyebrows showing personality
- Slight smirks and attitude in expressions
- Natural asymmetric poses
- Keeps friendly, approachable manga charm

### In Progress
- ⏳ German Shepherd remaining states (alert, sad, sleeping, eating, playing)
- ⏳ All other breeds (6 breeds x 8 states)

### Legacy Assets
Old manga-style single-sprite assets are still in this directory with original names:
- `german_shepherd.svg` (legacy)
- `rottweiler.svg` (legacy)
- `doberman.svg` (legacy)
- `bulldog.svg` (legacy)
- `pitbull.svg` (legacy)
- `husky.svg` (legacy)
- `beagle.svg` (legacy)

These will be used as fallbacks until all urban comic sprites are complete.

## Technical Specifications

### Canvas
- **Size**: 400x400px
- **ViewBox**: `0 0 400 400`
- **Format**: SVG (vector)

### Key Features (Manga + Fritz Hybrid)
- **Big manga eyes**: Expressive with multiple highlights + hooded/knowing looks
- **Bold outlines**: 3-5px dark brown/black strokes added to manga shapes
- **Soft rounded shapes**: Manga ellipses and curves (KEPT)
- **Expressive faces**: Real emotion, personality, attitude
- **Friendly colors**: Warm manga browns with subtle urban touches
- **Fritz personality**: Smirks, raised eyebrows, confident stances
- **Anthropomorphic expressions**: Street-smart without losing cuteness

### Performance Considerations
- Keep file size under 50KB per sprite
- Minimize filter usage
- Use simple paths over complex shapes where possible
- Avoid embedded images
- Use viewBox for responsive scaling

## Integration

### Code Integration
The `DogBreed` extension in `lib/models/dog.dart` provides:

```dart
// Get state-specific asset path
String path = DogBreed.germanShepherd.getAssetPathForState(DogAnimationState.barking);
// Returns: 'assets/dogs/german_shepherd_barking.svg'

// Legacy method (returns idle state)
String path = DogBreed.germanShepherd.assetPath;
// Returns: 'assets/dogs/german_shepherd_idle.svg'
```

### Widget Usage
The `AnimatedDogWidget` automatically loads the correct sprite based on animation state:

```dart
AnimatedDogWidget(
  breed: DogBreed.germanShepherd,
  controller: animationController,
  size: 200,
)
```

The widget will:
1. Try to load state-specific sprite (e.g., `german_shepherd_barking.svg`)
2. Fall back to legacy single sprite if state-specific not found
3. Show urban-themed loading placeholder while loading

## Contributing Sprites

### For Artists/Designers

1. **Reference the style guide** (`URBAN_COMIC_STYLE_GUIDE.md`) closely
2. **Start with idle state** - this is the primary sprite
3. **Use German Shepherd sprites as reference** for quality and style
4. **Follow naming convention** exactly
5. **Test at 200x200px** display size
6. **Ensure readability** on dark backgrounds

### Recommended Workflow

1. Sketch the dog breed in urban comic style
2. Create the idle pose first (most important)
3. Create barking pose next (most dramatic)
4. Create happy pose (shows personality)
5. Fill in remaining states
6. Test all states with transforms (AnimatedDogWidget applies scale/rotation)
7. Optimize SVG code
8. Submit sprites

### Quality Checklist
- [ ] Matches urban comic aesthetic
- [ ] Bold 4-6px outlines throughout
- [ ] Expressive face with personality
- [ ] Breed is recognizable
- [ ] Works on dark background (#0A0B0D)
- [ ] File size under 50KB
- [ ] Valid SVG with 400x400 viewBox
- [ ] Appropriate content (street-smart but not explicit)

## Animation System

### State Transitions
The app handles smooth transitions between states using Flutter transforms:
- **Scale** (breathing, emphasis)
- **Rotation** (head tilt, wiggle)
- **Opacity** (fading, sleepy)

### Timing
- **Idle**: 2s loop (gentle breathing)
- **Alert**: 0.3s quick transition
- **Barking**: 0.5s aggressive loop
- **Happy**: 0.6s bouncy loop
- **Sad**: 0.8s slow fade
- **Sleeping**: 3s slow breathing
- **Eating**: 3s cycle
- **Playing**: 2.5s energetic loop

## Urban Aesthetic Requirements

### Color Palette
- **Blacks**: #000000, #1A1A1A
- **Browns**: #3A2B1F, #8B6914
- **Grays**: #2B2D2F, #5C5C5C
- **Neon accents**: #00F5FF (cyan), #FF10F0 (pink)

### Visual Effects
- Solid black shadows (noir)
- Halftone dot patterns
- Motion lines for action
- Neon glow on collars/tags
- Comic panel composition

### Personality Traits
Each breed should exhibit:
- Street-smart attitude
- Real emotion (not cute)
- Urban cool factor
- Slight imperfections (scars, attitude)
- Confident body language

## Future Enhancements

### Planned Features
- [ ] Rive animations (smooth interpolation)
- [ ] Seasonal variations (rain, snow)
- [ ] Accessories system (bandanas, collars)
- [ ] Breed variations (different coat patterns)
- [ ] Special mood states (grumpy, excited)

### Optimization
- [ ] Sprite atlas for better performance
- [ ] WebP versions for web deployment
- [ ] Compressed SVG variants
- [ ] Lazy loading strategy

## Related Issues
- **Issue #41**: Underground comic art style for dog sprites
- **Issue #40**: EPIC - Urban Street Dog Aesthetic & Personality Overhaul
- **Issue #46**: UI redesign with underground comic aesthetic

## Questions?
Refer to `URBAN_COMIC_STYLE_GUIDE.md` or check the German Shepherd reference sprites for visual guidance.

---

**Last Updated**: 2025-10-29
**Version**: 1.0
**Status**: In Development (3/56 sprites completed)
