# Manga-Fritz Hybrid Dog Sprite Style Guide

## Overview
This style guide defines how to blend the existing **friendly Japanese manga aesthetic** with **Fritz the Cat's streetwise personality** for all dog sprites. We keep the approachable, cute manga base while adding an edge of urban attitude and character.

## Core Philosophy
**"Friendly street-smart" - Manga cute meets urban cool**

Think: A manga dog who's seen the neighborhood, knows the streets, has personality and attitude, but is still your loyal friend.

## Visual Style Blend

### Base: Japanese Manga Style (KEEP)
✅ Soft, rounded shapes (ellipses, rounded rectangles)
✅ Big expressive manga eyes with multiple highlights
✅ Gentle, warm color palette
✅ Friendly, approachable silhouettes
✅ Smooth curves and soft edges
✅ Background circles/halos

### Add: Fritz the Cat Characteristics
✨ **Bold variable outlines** (2-4px strokes on shapes)
✨ **Expressive, knowing looks** (eyes show personality)
✨ **Anthropomorphic expressions** (smirks, skeptical looks, attitude)
✨ **Street-smart body language** (confident stances, slight slouches)
✨ **Personality-driven eyebrows** (raised, expressive, character-showing)
✨ **Subtle edge** (remove overly cutesy elements like blush marks in some states)

### Remove/Minimize:
❌ Overly precious elements (excessive sparkles, musical notes)
❌ Baby-cute features (in favor of confident personality)
❌ Stiff, symmetrical poses (add natural weight shifts)

## Design Principles

### 1. Eyes - The Personality Window

**Manga Base (KEEP):**
- Large, expressive eyes (22-24px radius)
- Multiple layers: white → iris → pupil
- Bright manga highlights (2-3 spots)
- Oval/ellipse shape

**Fritz Enhancement (ADD):**
- **Hooded/lidded looks** - add upper eyelid line for attitude
- **Asymmetric expressions** - one eye slightly different
- **Knowing glances** - pupils positioned to show awareness
- **Expressive eyebrows** - raised, furrowed, skeptical
- **Eye shape variations** - slightly narrowed for cool looks

**Example Variations:**
```
Idle: Relaxed, slightly hooded eyes (casual, been there)
Alert: Wide, focused (street-smart awareness)
Happy: Big and bright (genuine manga joy)
Barking: Intense, focused (protective)
```

### 2. Outlines - Bold but Friendly

**Manga Base:**
- Minimal outlines (original has stroke-width: 2-3)
- Soft edges

**Fritz Enhancement:**
- **Add bold strokes** to main shapes (3-5px)
- **Variable line weight**:
  - Thicker (4-5px) on bottoms/shadows
  - Medium (3px) on main outlines
  - Thinner (2px) on details
- **Keep rounded** (stroke-linecap: round, stroke-linejoin: round)
- **Color**: Dark brown/black (#2d2d2d, #1a1a1a) not pure black

### 3. Body Language - Confident but Approachable

**Manga Base (KEEP):**
- Rounded body shapes
- Soft, gentle curves
- Friendly proportions

**Fritz Enhancement (ADD):**
- **Weight shifts** - lean to one side, not perfectly centered
- **Hip drops** - casual, confident stance
- **Head tilts** - personality and attitude
- **Asymmetric poses** - natural, lived-in feel
- **Slight slouches** - cool and casual, not stiff

### 4. Expressions - More Than Just Cute

**Idle State:**
- Manga: Sweet, content smile
- Fritz Add: Slight smirk, knowing look, "I've got this" vibe
- Eyes: Relaxed but aware, slightly hooded

**Alert State:**
- Manga: Attentive, wide eyes
- Fritz Add: Street-smart focus, one eyebrow raised
- Expression: "I see you" awareness

**Happy State:**
- Manga: Big smile, joy
- Fritz Add: Genuine but cool happiness, tongue out
- Keep the manga brightness but add personality

**Barking State:**
- Manga: Determined
- Fritz Add: Protective swagger, not just aggressive
- Show: "This is MY territory" confidence

### 5. Color Palette - Warm Manga + Urban Edge

**Keep the Manga Colors:**
- Warm browns: #c8956f, #d4a574, #a0824f
- Soft whites: #ffffff, #f0d5a8
- Gentle shadows: opacity 0.6-0.7

**Urban Enhancements:**
- Slightly desaturate in shadow areas
- Add subtle neon accents on collars: #4a9eff, #ffd700
- Background circles: Keep but make slightly muted

**Avoid:**
- Pure black (#000000) - use #1a1a1a, #2d2d2d instead
- Overly bright neons everywhere - just accent touches

### 6. Details - Personality Touches

**Keep from Manga:**
- Soft chest fur details
- Breed-specific markings
- Gentle highlights

**Add Fritz Elements:**
- **Collar tags** - subtle urban detail
- **Subtle scars** - lived experience (small ear notches, etc.)
- **Expression wrinkles** - smile lines, brow furrows
- **Natural imperfections** - slightly uneven ears, asymmetry

**Remove:**
- Musical notes (unless character-appropriate)
- Excessive blush marks (keep minimal)
- Hearts and overly cute decorations

## Technical Specifications

### Canvas
- **Size**: 400x400px
- **ViewBox**: `0 0 400 400`
- **Format**: SVG

### Structure (Keep from Manga)
```svg
<!-- Background circle -->
<!-- Body (ellipses) -->
<!-- Legs (rounded rects) -->
<!-- Head (ellipse) -->
<!-- Ears (paths or ellipses) -->
<!-- Markings -->
<!-- Snout -->
<!-- Nose -->
<!-- Mouth -->
<!-- Eyes (layered ellipses) -->
<!-- Eyebrows -->
<!-- Details -->
```

### Outline Strokes (ADD)
```svg
<!-- Add to main body shapes -->
stroke="#2d2d2d"
stroke-width="4"
stroke-linecap="round"
stroke-linejoin="round"

<!-- Detail strokes -->
stroke-width="2-3"
```

## Breed Personalities (Manga + Fritz)

### German Shepherd
- **Manga**: Noble, loyal, protective
- **Fritz Add**: Veteran street cop, seen it all, quietly confident
- **Eyes**: Slightly hooded, knowing look
- **Stance**: Relaxed but ready

### Rottweiler
- **Manga**: Strong, protective
- **Fritz Add**: Neighborhood bouncer, tough but fair
- **Eyes**: Direct gaze, steady
- **Expression**: Slight underbite showing character

### Doberman
- **Manga**: Sleek, elegant
- **Fritz Add**: Street racer, sharp and quick
- **Eyes**: Alert, focused
- **Stance**: Lean and ready

### Bulldog
- **Manga**: Sturdy, determined
- **Fritz Add**: Grizzled bartender, unmovable wisdom
- **Eyes**: Seen-it-all look
- **Expression**: Slight smirk

### Pitbull
- **Manga**: Friendly, loyal, strong
- **Fritz Add**: Misunderstood softie with muscle
- **Eyes**: Warm despite tough exterior
- **Stance**: Relaxed confidence

### Husky
- **Manga**: Energetic, playful
- **Fritz Add**: Sarcastic comedian, self-aware
- **Eyes**: Expressive eyebrows, knowing look
- **Expression**: Slight grin

### Beagle
- **Manga**: Curious, sweet
- **Fritz Add**: Scrappy street kid, clever
- **Eyes**: Alert, mischievous
- **Stance**: Wiry energy

## Animation States Guide

### Idle
- **Base**: Casual standing, gentle breathing
- **Add**: Weight on one leg, slight head tilt, knowing expression
- **Eyes**: Relaxed, slightly hooded

### Alert
- **Base**: Attentive, ears up
- **Add**: One eyebrow raised, street-smart focus
- **Eyes**: Wide but not scared, aware

### Barking
- **Base**: Mouth open, determined
- **Add**: Protective swagger, territorial confidence
- **Expression**: "Back off" attitude

### Happy
- **Base**: Big smile, tail wagging
- **Add**: Cool joy, tongue out, genuine happiness with personality
- **Eyes**: Bright manga sparkle with character

### Sad
- **Base**: Droopy, low energy
- **Add**: Disappointed rather than devastated, still has dignity
- **Eyes**: Downcast but not pathetic

### Sleeping
- **Base**: Peaceful rest
- **Add**: Sprawled casually, relaxed confidence even asleep
- **Details**: Slight snoring indication (Z's optional)

### Eating
- **Base**: Head down, enjoying food
- **Add**: Enthusiastic but not desperate, savoring
- **Expression**: Content focus

### Playing
- **Base**: Energetic, bouncy
- **Add**: Playful attitude with personality, not just cute
- **Stance**: Dynamic, fun but cool

## Step-by-Step Creation Process

### 1. Start with Existing Manga Sprite
- Use the original manga SVG as base
- Keep all the friendly shapes and proportions
- Keep the big manga eyes structure

### 2. Add Bold Outlines
```svg
<!-- Add stroke to body -->
<ellipse cx="200" cy="280" rx="90" ry="75"
         fill="#c8956f"
         stroke="#2d2d2d"
         stroke-width="4"/>
```

### 3. Enhance Eyes
- Keep manga size and structure
- Add upper eyelid line for hood
- Adjust pupil position for personality
- Enhance eyebrow expressiveness

### 4. Adjust Pose
- Shift body slightly off-center
- Tilt head for character
- Adjust weight distribution
- Add asymmetry

### 5. Update Expression
- Modify mouth to slight smirk/knowing look
- Adjust eyebrows for personality
- Add expression lines (subtle)

### 6. Add Fritz Details
- Small ear notch or character mark
- Subtle collar details
- Expression wrinkles
- Natural asymmetry

### 7. Test on Dark Background
- Ensure readability
- Check outline visibility
- Verify personality shows through

## Quality Checklist

- [ ] Keeps friendly manga aesthetic
- [ ] Big expressive manga eyes
- [ ] Soft rounded shapes maintained
- [ ] Bold outlines added (3-5px)
- [ ] Shows personality/attitude
- [ ] Natural, asymmetric pose
- [ ] Expressive eyebrows
- [ ] Knowing/streetwise expression
- [ ] Breed recognizable
- [ ] Works on light or dark backgrounds
- [ ] Appropriate content
- [ ] File size under 75KB

## What Success Looks Like

**Perfect Blend:**
- At first glance: "That's a cute manga dog!"
- On second look: "Wait, that dog has personality and attitude!"
- Overall feel: Friendly and approachable, but with character and street smarts
- Not: Harsh or aggressive, but confident
- Not: Baby-cute, but cool-cute

## Examples to Reference

**Manga Influence:**
- Original beagle.svg, german_shepherd.svg (our base)
- Big eyes, soft shapes, friendly feel

**Fritz the Cat Influence:**
- Anthropomorphic expressions
- Knowing looks and smirks
- Street-smart body language
- Variable line weight
- Character-driven design

**NOT:**
- Pure underground comix (too harsh)
- Generic pet illustrations (no personality)
- Overly realistic (loses charm)

## Common Mistakes to Avoid

❌ Making it too edgy (loses manga charm)
❌ Removing all cuteness (we want friendly)
❌ Symmetrical, stiff poses (add natural asymmetry)
❌ Tiny thin lines (need bold outlines)
❌ Pure black everywhere (use dark browns)
❌ Losing breed recognition
❌ Overly complex (keep manga simplicity)

## Final Notes

This style is about **adding personality to existing cuteness**, not replacing it. Think of it as:

> "These are manga dogs who've grown up in the neighborhood. They're still friendly and approachable, but they've got street smarts, personality, and confidence. They're cool without being mean, and cute without being precious."

The goal: Dogs you want to hang out with AND who can protect your car.

---

**Version**: 2.0 (Manga-Fritz Hybrid)
**Last Updated**: 2025-10-29
**Replaces**: URBAN_COMIC_STYLE_GUIDE.md (v1.0)
