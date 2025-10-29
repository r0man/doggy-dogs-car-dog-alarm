# Urban Underground Comic Dog Sprite Style Guide

## Overview
This guide defines the visual aesthetic for all dog sprites in the Doggy Dogs Car Alarm app. The style is inspired by underground comics (Robert Crumb, Fritz the Cat) with a street-smart, edgy attitude while remaining appropriate.

## Core Design Principles

### 1. Bold Outlines with Variable Line Weight
- **Primary outlines**: 4-6px black strokes
- **Secondary details**: 2-3px black strokes
- **Emphasis lines**: 6-8px for dramatic effect
- **Variable weight**: Thicker at bottom/shadow areas, thinner at highlights
- **No anti-aliasing** on outlines for crisp comic book feel

### 2. Expressive Faces with Real Emotion
- **Large, expressive eyes** with attitude
  - Hooded/skeptical expressions
  - Side-eye glances
  - Narrowed eyes for suspicion
  - Wide-eyed for excitement
- **Dynamic eyebrows** that convey emotion
- **Mouths with personality**
  - Slight smirks
  - Crooked grins
  - Snarls when appropriate
  - Tongue lolling for happy/playful
- **No cutesy sparkles or manga eyes** - keep it real and gritty

### 3. Exaggerated Poses with Attitude
- **Stance**: Slightly slouched, cool confidence
- **Weight distribution**: Asymmetric, one hip dropped
- **Head angles**: Slight tilts for personality
- **Body language**: Street-smart swagger
- **Avoid**: Symmetrical "standing at attention" poses

### 4. Color Palette (Urban Night Theme)
- **Base colors**: Realistic dog colors but slightly desaturated
- **Shadows**: Deep blacks and dark grays (no soft gradients)
- **Highlights**: Sharp, high-contrast highlights
- **Accent colors**: Match urban theme
  - Concrete grays: #2B2D2F, #4A4E52
  - Rust orange: #E85D27
  - Neon accents for special effects: #00F5FF, #FF10F0
- **Background**: Transparent or night sky (#0A0B0D)

### 5. Comic Book Rendering Techniques

#### Halftone Shading
- Use circular halftone dots for shadows
- Dot spacing: 6-8px apart
- Dot size: 3-5px diameter
- Applied to: Shadows, mid-tones

#### Cross-Hatching
- Use for texture and depth
- 45-degree angle lines
- Variable density for shadow intensity
- Line spacing: 4-6px

#### Solid Blacks
- Large shadow areas should be solid black
- Creates strong contrast and drama
- Noir-style lighting

### 6. Street-Smart Personality Traits

Each breed should have these urban personality markers:

**German Shepherd**: Seasoned veteran cop-dog, seen it all
- Skeptical expression
- Battle-scarred (small notch in ear)
- Confident stance

**Rottweiler**: Tough bouncer, but friendly to regulars
- Strong silhouette
- Slight underbite showing teeth
- Protective posture

**Doberman**: Sleek street racer, fast and sharp
- Lean and angular
- Alert, focused eyes
- Aerodynamic pose

**Bulldog**: Grizzled bartender, wise and unmovable
- Thick-set with attitude
- Cigar or toothpick in mouth (when appropriate)
- Immovable stance

**Pitbull**: Misunderstood softie, loyal muscle
- Broad shoulders, confident
- Warm eyes despite tough exterior
- Relaxed but powerful pose

**Husky**: Sarcastic comedian, always has a comment
- Expressive eyebrows
- Slightly goofy but self-aware
- Dynamic, energetic pose

**Beagle**: Scrappy street kid, clever and resourceful
- Oversized ears, alert
- Wiry, energetic stance
- Mischievous expression

## Animation States

### Idle (Default)
- Casual stance, weight on one leg
- Slight breathing motion
- Occasional ear twitch or eye movement
- Cool, relaxed attitude

### Alert (Vigilant)
- Ears perked up
- Eyes focused, slightly narrowed
- Body tensed, ready to move
- Serious expression

### Barking (Aggressive)
- Mouth wide open showing teeth
- Body lunging forward
- Fur bristling (indicated by rough edges)
- Intense, focused eyes
- Action lines for motion

### Happy (Joyful)
- Tongue out, panting smile
- Eyes bright and wide
- Tail wagging (motion blur lines)
- Bouncy, energetic pose
- Relaxed body language

### Sad (Low Energy)
- Droopy ears
- Downcast eyes
- Slumped posture
- Reduced detail/contrast
- Slow breathing

## Technical Specifications

### Canvas Size
- **Dimensions**: 400x400px
- **ViewBox**: 0 0 400 400
- **Resolution**: Vector (SVG)

### File Naming Convention
```
{breed}_{state}.svg

Examples:
- german_shepherd_idle.svg
- rottweiler_barking.svg
- bulldog_happy.svg
```

### Layer Structure
1. Background (if any)
2. Shadow layer
3. Body base colors
4. Body details
5. Face features
6. Outlines (top layer)
7. Effects (motion lines, impact stars, etc.)

### SVG Best Practices
- Use `<path>` for main shapes (better control)
- Group related elements in `<g>` tags
- Keep stroke width consistent within element groups
- Use `stroke-linejoin="round"` and `stroke-linecap="round"` for outlines
- Minimize use of filters (keep performance high)

## Reference Material

### Underground Comics Style
- **Robert Crumb**: Variable line weight, expressive faces
- **Fritz the Cat**: Anthropomorphic attitude, street scenes
- **Tank Girl**: Punk aesthetic, bold lines
- **Adult Swim aesthetic**: Edgy but accessible

### What to AVOID
❌ Cute/kawaii aesthetics
❌ Pastel colors
❌ Rounded, soft edges everywhere
❌ Symmetrical, stiff poses
❌ Generic stock pet illustrations
❌ Overly realistic rendering
❌ Gradient-heavy designs

### What to EMBRACE
✅ Bold black outlines
✅ High contrast
✅ Attitude and personality
✅ Street-smart expressions
✅ Exaggerated features
✅ Comic book rendering techniques
✅ Night-time urban atmosphere

## Color Swatches

### Dog Coat Colors (Desaturated for urban feel)
- Black: #1A1A1A
- Brown: #3A2B1F
- Tan: #A68B5B
- Gray: #5C5C5C
- White: #E8E8E8 (not pure white)
- Rust: #8B4513

### Accent Colors
- Neon Cyan (eyes, collar): #00F5FF
- Neon Pink (highlights): #FF10F0
- Rust Orange (details): #E85D27
- Concrete Gray (shadows): #2B2D2F

## Example Compositions

### Idle Pose Example
```
- Dog standing at 3/4 view
- Weight shifted to back leg
- Front leg slightly bent
- Head turned toward viewer
- One ear slightly more alert than other
- Mouth closed or slight smirk
- Eyes half-lidded, cool and calm
```

### Barking Pose Example
```
- Full frontal view
- Body lunged forward
- Mouth wide open, teeth showing
- Eyes intense and focused
- Fur bristling (jagged outline)
- Motion lines from mouth
- Speed lines behind body
- Paws planted firmly
```

## Implementation Notes

- All sprites should work on dark backgrounds (night theme)
- Add subtle neon glow effects to collars or eyes for night-time emphasis
- Consider adding optional "urban accessories" (bandana, collar with spikes, etc.)
- Maintain breed recognition while adding personality
- Test readability at 200x200px display size

## Approval Checklist

Before finalizing sprites, verify:
- [ ] Bold outlines (4-6px) throughout
- [ ] Expressive face with attitude
- [ ] Asymmetric, natural pose
- [ ] High contrast, no muddy mid-tones
- [ ] Street-smart personality evident
- [ ] Works on dark background
- [ ] Recognizable as specific breed
- [ ] Appropriate (no explicit content)
- [ ] Matches animation state clearly
- [ ] Consistent with other breed sprites

---

**Version**: 1.0
**Last Updated**: 2025-10-29
**Related Issue**: doggy-dogs-car-dog-alarm-41
