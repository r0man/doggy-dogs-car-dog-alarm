import 'dart:math';
import 'dog.dart';
import 'neighborhood.dart';

/// Enhanced personality archetype
enum PersonalityArchetype {
  streetSmart,
  reformedBadDog,
  overachiever,
  zenMaster,
  conspiracyTheorist,
  theArtist,
  veteran,
  rookie,
}

extension PersonalityArchetypeExtension on PersonalityArchetype {
  String get displayName {
    switch (this) {
      case PersonalityArchetype.streetSmart:
        return 'Street Smart';
      case PersonalityArchetype.reformedBadDog:
        return 'Reformed Bad Dog';
      case PersonalityArchetype.overachiever:
        return 'Overachiever';
      case PersonalityArchetype.zenMaster:
        return 'Zen Master';
      case PersonalityArchetype.conspiracyTheorist:
        return 'Conspiracy Theorist';
      case PersonalityArchetype.theArtist:
        return 'The Artist';
      case PersonalityArchetype.veteran:
        return 'The Veteran';
      case PersonalityArchetype.rookie:
        return 'The Rookie';
    }
  }

  String get description {
    switch (this) {
      case PersonalityArchetype.streetSmart:
        return 'Knows the game. Reads situations. Always three steps ahead.';
      case PersonalityArchetype.reformedBadDog:
        return 'Had a wild past. Changed their ways. Trying to do good now.';
      case PersonalityArchetype.overachiever:
        return 'Takes the job seriously. Maybe too seriously. Excellence is mandatory.';
      case PersonalityArchetype.zenMaster:
        return 'Calm. Centered. Nothing rattles them. Inner peace and outer vigilance.';
      case PersonalityArchetype.conspiracyTheorist:
        return 'Sees patterns everywhere. Connects dots others miss. Suspicious of everything.';
      case PersonalityArchetype.theArtist:
        return 'Philosophical. Observant. Finds beauty in the urban landscape.';
      case PersonalityArchetype.veteran:
        return 'Seen it all. Done it twice. Battle-tested and wise.';
      case PersonalityArchetype.rookie:
        return 'New to this. Eager. Still learning the ropes.';
    }
  }
}

/// Former occupation
enum FormerOccupation {
  streetDog,
  showDog,
  junkyardDog,
  policeDog,
  therapyDog,
  movieDog,
  rescueDog,
}

extension FormerOccupationExtension on FormerOccupation {
  String get displayName {
    switch (this) {
      case FormerOccupation.streetDog:
        return 'Street Dog';
      case FormerOccupation.showDog:
        return 'Show Dog';
      case FormerOccupation.junkyardDog:
        return 'Junkyard Dog';
      case FormerOccupation.policeDog:
        return 'Police Dog';
      case FormerOccupation.therapyDog:
        return 'Therapy Dog';
      case FormerOccupation.movieDog:
        return 'Movie Dog';
      case FormerOccupation.rescueDog:
        return 'Rescue Dog';
    }
  }

  String get backstory {
    switch (this) {
      case FormerOccupation.streetDog:
        return 'Grew up on the streets. Self-made. Learned by doing.';
      case FormerOccupation.showDog:
        return 'Competed in shows. Left the glamour behind. Wanted real work.';
      case FormerOccupation.junkyardDog:
        return 'Protected junkyards. Tough work. Built character.';
      case FormerOccupation.policeDog:
        return 'K-9 unit. Honorable discharge. Still got the instincts.';
      case FormerOccupation.therapyDog:
        return 'Helped people heal. Good work, but needed a change.';
      case FormerOccupation.movieDog:
        return 'Hollywood stunt double. Fame faded. Found purpose elsewhere.';
      case FormerOccupation.rescueDog:
        return 'Search and rescue. Saved lives. Now saves cars.';
    }
  }
}

/// Dog backstory with personality
class DogBackstory {
  final String name;
  final DogBreed breed;
  final Neighborhood originNeighborhood;
  final PersonalityArchetype archetype;
  final FormerOccupation occupation;
  final String definingMoment;
  final String personalVendetta;
  final String futureDream;
  final String tagline;

  const DogBackstory({
    required this.name,
    required this.breed,
    required this.originNeighborhood,
    required this.archetype,
    required this.occupation,
    required this.definingMoment,
    required this.personalVendetta,
    required this.futureDream,
    required this.tagline,
  });

  /// Generate random backstory
  factory DogBackstory.generate({
    required String name,
    required DogBreed breed,
    int? seed,
  }) {
    final random = seed != null ? Random(seed) : Random();

    final origin = Neighborhood.values[random.nextInt(Neighborhood.values.length)];
    final archetype =
        PersonalityArchetype.values[random.nextInt(PersonalityArchetype.values.length)];
    final occupation = FormerOccupation.values[random.nextInt(FormerOccupation.values.length)];

    final definingMoments = [
      'Once caught a car thief red-handed. Changed everything.',
      'Witnessed a hit-and-run. Vowed to protect.',
      'Lost a previous owner to theft. Never again.',
      'Saved a kid from a break-in. Found their calling.',
      'Grew up watching cars get stolen. Decided to fight back.',
      'Used to ignore crime. One day, couldn\'t anymore.',
    ];

    final vendettas = [
      'Hates litterers. Absolutely hates them.',
      'Can\'t stand car break-ins. Personal mission.',
      'Despises fake car alarms. They\'re insulting.',
      'Has beef with seagulls. Long story.',
      'Suspicious of anyone in hoodies. Probably unfair.',
      'Thinks parking meter officers are too strict.',
      'Believes pigeons are undercover agents.',
    ];

    final dreams = [
      'Dreams of retiring to Waterfront. Peaceful dock life.',
      'Wants to open a guard dog training school someday.',
      'Plans to write a book: "Car Security for Dummies".',
      'Saving up for a trip to the Outskirts. Hear it\'s nice.',
      'Hopes to eventually move to Uptown. Live the good life.',
      'Wants to start a podcast about street philosophy.',
      'Dreams of a world with no car crime. Idealistic, maybe.',
    ];

    final definingMoment = definingMoments[random.nextInt(definingMoments.length)];
    final vendetta = vendettas[random.nextInt(vendettas.length)];
    final dream = dreams[random.nextInt(dreams.length)];

    final tagline = _generateTagline(breed, origin, archetype);

    return DogBackstory(
      name: name,
      breed: breed,
      originNeighborhood: origin,
      archetype: archetype,
      occupation: occupation,
      definingMoment: definingMoment,
      personalVendetta: vendetta,
      futureDream: dream,
      tagline: tagline,
    );
  }

  static String _generateTagline(
      DogBreed breed, Neighborhood origin, PersonalityArchetype archetype) {
    return '${breed.displayName} from ${origin.displayName}. ${archetype.displayName}.';
  }

  /// Get full backstory text
  String getFullStory() {
    return '''
$tagline

Origin: ${originNeighborhood.displayName}
Background: ${occupation.backstory}

${archetype.description}

$definingMoment

$personalVendetta

$futureDream
''';
  }

  /// Get short bio
  String getShortBio() {
    return '$tagline\nFormer ${occupation.displayName}. $personalVendetta';
  }

  /// Convert to JSON
  Map<String, dynamic> toJson() {
    return {
      'name': name,
      'breed': breed.name,
      'originNeighborhood': originNeighborhood.name,
      'archetype': archetype.name,
      'occupation': occupation.name,
      'definingMoment': definingMoment,
      'personalVendetta': personalVendetta,
      'futureDream': futureDream,
      'tagline': tagline,
    };
  }

  /// Create from JSON
  factory DogBackstory.fromJson(Map<String, dynamic> json) {
    return DogBackstory(
      name: json['name'] as String,
      breed: DogBreed.values.firstWhere((e) => e.name == json['breed']),
      originNeighborhood: Neighborhood.values.firstWhere(
        (e) => e.name == json['originNeighborhood'],
      ),
      archetype: PersonalityArchetype.values.firstWhere(
        (e) => e.name == json['archetype'],
      ),
      occupation: FormerOccupation.values.firstWhere(
        (e) => e.name == json['occupation'],
      ),
      definingMoment: json['definingMoment'] as String,
      personalVendetta: json['personalVendetta'] as String,
      futureDream: json['futureDream'] as String,
      tagline: json['tagline'] as String,
    );
  }
}
