import 'dart:math';
import '../models/dog.dart';
import '../models/dialogue_context.dart';

/// Service for generating context-aware witty dialogue
/// Fritz the Cat-inspired street-smart commentary
class DialogueService {
  final Random _random = Random();

  /// Get a witty response based on context, dog personality, and situation
  String getDialogue({
    required Dog dog,
    required DialogueData data,
  }) {
    // Select response pool based on context
    List<String> responses;

    switch (data.context) {
      case DialogueContext.alarmActivated:
        responses = _getAlarmActivatedResponses(dog);
      case DialogueContext.checkIn:
        responses = _getCheckInResponses(dog);
      case DialogueContext.threatDetected:
        responses = _getThreatDetectedResponses(dog, data.threatLevel ?? 5);
      case DialogueContext.falseAlarm:
        responses = _getFalseAlarmResponses(dog);
      case DialogueContext.userReturned:
        responses = _getUserReturnedResponses(dog, data.timeGone);
      case DialogueContext.feeding:
        responses = _getFeedingResponses(dog);
      case DialogueContext.playing:
        responses = _getPlayingResponses(dog);
      case DialogueContext.neglected:
        responses = _getNeglectedResponses(dog);
      case DialogueContext.alarmSuccess:
        responses = _getAlarmSuccessResponses(dog);
      case DialogueContext.goneTooLong:
        responses = _getGoneTooLongResponses(dog, data.timeGone);
      case DialogueContext.parkingLocation:
        responses = _getParkingLocationResponses(dog, data.locationDescription);
      case DialogueContext.morning:
        responses = _getMorningResponses(dog);
      case DialogueContext.evening:
        responses = _getEveningResponses(dog);
      case DialogueContext.levelUp:
        responses = _getLevelUpResponses(dog, data.newLevel ?? dog.level);
      case DialogueContext.achievement:
        responses =
            _getAchievementResponses(dog, data.achievementName ?? 'something');
    }

    // Personality modifications
    responses = _applyPersonalityModifier(responses, dog.personality);

    // Return random response from pool
    return responses[_random.nextInt(responses.length)];
  }

  // ALARM ACTIVATED RESPONSES
  List<String> _getAlarmActivatedResponses(Dog dog) {
    return [
      "Alright, I got this. Nobody's touching ${_possessivePronoun(dog)}.",
      'On duty. This is what I do.',
      "Let's see who wants to try me today.",
      'Guard mode: activated. Coffee would be nice, but whatever.',
      "Yep, I'm watching. Not my first rodeo.",
      "I'll keep an eye out. You go do your thing.",
      "Car security? That's literally my whole deal.",
      "Don't worry. I've scared off bigger threats than mall parking lots.",
      "Activated. Now go, before you're late again.",
      "I'm on it. Nobody's getting past this face.",
    ];
  }

  // CHECK-IN RESPONSES
  List<String> _getCheckInResponses(Dog dog) {
    if (dog.stats.happiness < 40) {
      return [
        "Oh, NOW you check in? I've been here the whole time.",
        'Just living my best life. Guarding stuff. You know, the usual.',
        "Yeah, I'm here. Still. Watching. Forever.",
        "Car's fine. Me? Eh, I've been better.",
        'Oh hey, remember me? Your guard dog?',
      ];
    } else if (dog.stats.happiness > 70) {
      return [
        "Hey! Everything's cool here. You good?",
        "All clear on this end. What's up?",
        "Car's safe, I'm great, life is good.",
        'Just posted up, keeping watch. You know how it is.',
        'Nothing to report. Just me and the street.',
      ];
    } else {
      return [
        "Status update: I'm a dog in a car. Shocking development.",
        "Yeah, everything's fine. The car hasn't grown legs yet.",
        'Still here, still watching. Living the dream.',
        'Oh you know, just doing dog stuff. Guarding things.',
        'Car status: still a car. Threat level: average Tuesday.',
      ];
    }
  }

  // THREAT DETECTED RESPONSES
  List<String> _getThreatDetectedResponses(Dog dog, int threatLevel) {
    if (threatLevel >= 8) {
      return [
        "YO! SOMEONE'S GETTING TOO CLOSE!",
        "This is NOT a drill! Someone's on the car!",
        "BACK OFF! This ain't your ride!",
        'We got a situation here! REAL threat!',
        'GET AWAY FROM THE CAR! I mean it!',
        'Not today, buddy! NOT TODAY!',
      ];
    } else if (threatLevel >= 5) {
      return [
        "Uh, someone's looking a little TOO interested in here.",
        'We got a lurker. Watching them closely.',
        "Someone's checking us out. Stay alert.",
        'Eyes on suspicious activity. Might bark about it.',
        'This person seems... questionable.',
      ];
    } else {
      return [
        'Someone walked by. Probably nothing.',
        "Random person. They're moving along.",
        "Just someone passing. I'm watching though.",
        'Noted and logged. Continuing surveillance.',
        'Pedestrian traffic. Normal street activity.',
      ];
    }
  }

  // FALSE ALARM RESPONSES
  List<String> _getFalseAlarmResponses(Dog dog) {
    return [
      'My bad. Thought that bag was a person.',
      'False alarm. Wind is apparently my nemesis.',
      'Okay, so it was just a shopping cart. Sue me.',
      'That shadow really looked suspicious. I stand by my vigilance.',
      'Better safe than sorry, right? ...Right?',
      "Look, plastic bags are sneaky. You don't know.",
      'In my defense, that pigeon was HUGE.',
      'Not a threat. I may have overreacted. Slightly.',
      'Sometimes you bark at leaves. It happens.',
      "I'm just thorough, okay? Very, very thorough.",
    ];
  }

  // USER RETURNED RESPONSES
  List<String> _getUserReturnedResponses(Dog dog, Duration? timeGone) {
    if (timeGone == null) {
      return [
        "Welcome back. Car's in one piece.",
        "You're back. Mission accomplished."
      ];
    }

    final hours = timeGone.inHours;
    final minutes = timeGone.inMinutes;

    if (hours >= 3) {
      return [
        "Three hours? What'd you do, buy the whole store?",
        "Long trip. Car's safe though. You're welcome.",
        'I was starting to think you moved out.',
        'Did you walk to the store and back? That was a while.',
        'Finally! I was about to file a missing person report.',
        'Long shopping trip, huh? Everything okay?',
      ];
    } else if (hours >= 1) {
      return [
        "Took your time, huh? Car's all good.",
        "An hour well spent, I hope. Everything's secure.",
        'Welcome back. Uneventful hour, thankfully.',
        "You're back. I barely missed you. ...Barely.",
        "Car: protected. Duty: done. Let's go.",
      ];
    } else if (minutes < 10) {
      return [
        'That was quick! Forget something?',
        "Already? Well, car's still here.",
        'Quick trip. Everything cool?',
        "Fast! I like it. Let's bounce.",
        "Barely had time to nap. Let's roll.",
      ];
    } else {
      return [
        "Back already? Cool. Car's fine.",
        'Hey! Everything good? Ready to go?',
        'Welcome back. All quiet on this front.',
        "You're back! Mission successful.",
        "Secured and delivered. Let's move.",
      ];
    }
  }

  // FEEDING RESPONSES
  List<String> _getFeedingResponses(Dog dog) {
    if (dog.stats.hunger < 30) {
      return [
        'FINALLY! I was wasting away here!',
        'Oh good, I only starved for like an hour.',
        'About time! Was gonna start eating the seats.',
        'Food! The thing I need to live! What a concept!',
        "You remembered! I'm touched. And hungry.",
      ];
    } else {
      return [
        'Hey, thanks! Appreciate it.',
        "Now we're talking. Good stuff.",
        "You know what? You're alright.",
        'This hits the spot. Thanks, boss.',
        "Nom nom nom. *chef's kiss*",
        'Okay, this is pretty good. Not gonna lie.',
        "You take care of me, I take care of the car. Deal's a deal.",
      ];
    }
  }

  // PLAYING RESPONSES
  List<String> _getPlayingResponses(Dog dog) {
    return [
      "Now THIS is what I'm talking about!",
      'Break time! I like it!',
      'Yes! Play first, guard later!',
      'This is the good stuff right here.',
      'You know how to treat a dog. Respect.',
      'I needed this. Thanks, really.',
      'Alright, alright! You got moves!',
      'This is way better than sitting in a car all day.',
      "We should do this more often. I'm just saying.",
      'Peak dog experience right here!',
    ];
  }

  // NEGLECTED RESPONSES
  List<String> _getNeglectedResponses(Dog dog) {
    return [
      "So... we're just not doing care anymore? Cool, cool.",
      "I'm fine. It's fine. Everything's fine. *sigh*",
      'Not to be dramatic, but I exist and have needs.',
      'Remember when you used to check on me? Good times.',
      "I'm just gonna be over here. Not eating. Or playing.",
      "It's cool. I'll just guard stuff while slowly fading away.",
      'Hey, quick question: do you still remember you have a dog?',
      "I'd be more effective if I wasn't, you know, neglected.",
    ];
  }

  // ALARM SUCCESS RESPONSES
  List<String> _getAlarmSuccessResponses(Dog dog) {
    return [
      "YEAH! That's right! Keep walking!",
      "And THAT'S how it's done!",
      'Did you SEE that?! I am GOOD at this!',
      "Threat: neutralized. I'll be here all week.",
      "That's what I DO! Car secured!",
      "Scared 'em off! Still got it!",
      'You picked the right dog for this job.',
      'BOOM! Another successful defense!',
      'I should get a raise for this. Or treats. Treats work.',
      "That's the stuff! This is why you keep me around!",
    ];
  }

  // GONE TOO LONG RESPONSES
  List<String> _getGoneTooLongResponses(Dog dog, Duration? timeGone) {
    final hours = timeGone?.inHours ?? 4;
    return [
      "It's been $hours hours. HOURS. Just FYI.",
      "Starting to think you're not coming back.",
      'Long shopping trip or did you move to another state?',
      "I've aged significantly since you left.",
      'Are you lost? Should I call someone?',
      'This is becoming a survival situation.',
      'Pretty sure I saw a tumbleweed go by.',
      "Fun fact: dogs can't tell time. But I KNOW it's been forever.",
    ];
  }

  // PARKING LOCATION RESPONSES
  List<String> _getParkingLocationResponses(Dog dog, String? location) {
    // Contextual responses based on location description
    final List<String> generic = [
      "Interesting spot. I'll make it work.",
      "Not where I'd park, but sure.",
      "This'll do. Seen worse.",
      'Bold choice. I respect it.',
      "Well, it's definitely... a location.",
    ];

    // Add specific responses if location contains certain keywords
    if (location?.toLowerCase().contains('shad') ?? false) {
      generic.addAll([
        'Under a tree? Really? Hope you like bird presents.',
        "Shady spot. Literally. At least it's cool.",
        'Tree parking. Watch out for falling branches. And birds.',
      ]);
    }

    if (location?.toLowerCase().contains('sun') ?? false) {
      generic.addAll([
        'Full sun? In summer? You trying to cook me?',
        "It's gonna be like an oven in here. Just saying.",
        'Sunny spot. Bold move. Hope you have AC.',
      ]);
    }

    if (location?.toLowerCase().contains('corner') ?? false) {
      generic.addAll([
        'Corner spot. Two directions to watch. Making my life hard.',
        "The corner? Tactical, I'll give you that.",
      ]);
    }

    return generic;
  }

  // MORNING RESPONSES
  List<String> _getMorningResponses(Dog dog) {
    return [
      "Morning. Let's do this.",
      'Another day, another car to guard.',
      "Up and at 'em. What's the plan?",
      'Morning! Ready to protect some stuff.',
      "New day, same gig. Let's roll.",
      'Bright and early. I respect the hustle.',
    ];
  }

  // EVENING RESPONSES
  List<String> _getEveningResponses(Dog dog) {
    return [
      'Evening shift. My favorite.',
      'Night patrol. This is when things get interesting.',
      'Evening! Time for the serious guarding.',
      'Sundown. Prime suspicious activity hours.',
      "Night time is the right time. Let's do this.",
      'After dark. When the real work happens.',
    ];
  }

  // LEVEL UP RESPONSES
  List<String> _getLevelUpResponses(Dog dog, int newLevel) {
    return [
      "Level $newLevel! I'm evolving!",
      'Leveled up! Getting better at this gig!',
      'New level! More skills, same attitude.',
      'Level $newLevel achieved! Still got it!',
      'Upgraded! Watch out, world.',
      "Level up! I'm officially more experienced.",
    ];
  }

  // ACHIEVEMENT RESPONSES
  List<String> _getAchievementResponses(Dog dog, String achievementName) {
    return [
      'Achievement unlocked: $achievementName! Not bad!',
      "Got a badge for '$achievementName.' I'll take it.",
      '$achievementName? EARNED.',
      "New achievement! '$achievementName.' Feels good.",
      'Unlocked: $achievementName. Add it to the collection.',
    ];
  }

  // PERSONALITY MODIFIERS
  List<String> _applyPersonalityModifier(
      List<String> responses, DogPersonality personality) {
    // Personality affects tone but not content
    // This is where we'd adjust language based on traits

    if (personality.nervous) {
      // Add nervous flair
      return responses.map((r) => r.replaceAll('!', '...')).toList();
    }

    if (personality.playful) {
      // Add enthusiasm
      return responses;
    }

    if (personality.lazy) {
      // More casual, less energy
      return responses.map((r) => r.replaceAll('!', '.')).toList();
    }

    return responses;
  }

  // Helper methods
  String _possessivePronoun(Dog dog) {
    // Simple version - could be expanded with dog gender if added
    return 'your ride';
  }
}
