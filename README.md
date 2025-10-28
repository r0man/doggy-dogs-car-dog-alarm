# Doggy Dogs Car Dog Alarm

A Flutter mobile app that turns your phone into a car security system with a virtual guard dog!

## Overview

Doggy Dogs Car Dog Alarm is an innovative car security app that uses your phone's sensors to detect potential break-ins and responds with realistic dog barking sounds to deter burglars. Leave your phone in the car, activate the app, and let your virtual guard dog watch over your vehicle.

## Features

### Core Functionality
- **Motion Detection**: Uses accelerometer and gyroscope sensors to detect car shaking or suspicious movement
- **Instant Alert Response**: Triggers loud dog barking when potential break-in is detected
- **Background Monitoring**: Continues to monitor sensors even when the screen is off

### Dog Customization
- **Breed Selection**: Choose from various dog breeds, each with unique characteristics
- **Bark Customization**: Select different bark styles (aggressive, warning, continuous, etc.)
- **Personality Traits**: Add funny characteristics to your virtual guard dog
  - Fierce protector
  - Nervous barker
  - Deep growler
  - Playful but alert
  - And more!

### Settings & Controls
- **Sensitivity Adjustment**: Configure how sensitive the motion detection should be
- **Volume Control**: Set the bark volume to your preference
- **Activation Timer**: Delay activation to give you time to exit the vehicle
- **Battery Optimization**: Smart power management to preserve phone battery

## How It Works

1. **Park Your Car**: Park and prepare to leave your vehicle
2. **Activate the App**: Open Doggy Dogs Car Dog Alarm
3. **Set Your Guard Dog**: Ensure your virtual dog is customized to your liking
4. **Enable Alarm**: Tap the activation button
5. **Leave Your Phone**: Place your phone securely in the car (ideally on a stable surface)
6. **Exit Safely**: Use the timer feature to exit before monitoring begins
7. **Auto-Protection**: The app monitors sensors continuously
8. **Bark Alert**: If suspicious movement is detected, your virtual dog barks loudly

## Sensor Technology

The app utilizes multiple phone sensors:

- **Accelerometer**: Detects changes in movement and vibration
- **Gyroscope**: Monitors orientation changes and tilting
- **Proximity Sensor**: Can detect if someone approaches the phone
- **Sound Level**: (Optional) Can detect breaking glass or loud noises

## Installation

### Prerequisites
- Flutter SDK (version 3.0 or higher)
- Dart SDK
- Android Studio / Xcode for mobile development
- A physical device for testing (sensors work best on real hardware)

### Setup

```bash
# Clone the repository
git clone https://github.com/yourusername/doggy-dogs-car-dog-alarm.git

# Navigate to the project directory
cd doggy-dogs-car-dog-alarm

# Install dependencies
flutter pub get

# Run the app
flutter run
```

## Usage Guide

### First Time Setup
1. Launch the app
2. Grant necessary permissions (sensors, audio, notifications)
3. Complete the dog customization tutorial
4. Test the sensitivity settings in a safe environment

### Daily Use
1. Open the app before leaving your car
2. Verify settings (volume, sensitivity)
3. Tap "Activate Guard Dog"
4. Exit the vehicle within the timer period
5. To deactivate, return and enter your unlock code/pattern

### Best Practices
- Place phone on a stable surface (dashboard, center console)
- Ensure phone has sufficient battery (50%+ recommended)
- Test in your driveway first to adjust sensitivity
- Consider connecting to car charger for extended protection
- Use in conjunction with actual car security systems

## Customization Options

### Available Dog Breeds
- German Shepherd
- Rottweiler
- Doberman
- Bulldog
- Pitbull
- Husky
- Beagle
- And many more!

### Bark Styles
- Single warning bark
- Aggressive continuous barking
- Deep growl with intermittent barks
- Multi-pitch bark sequence
- Territorial warning pattern

### Additional Features
- Custom dog names
- Visual dog animation on screen
- Bark intensity levels
- False alarm prevention settings

## Technical Stack

- **Framework**: Flutter/Dart
- **Sensors**: sensors_plus package
- **Audio**: audioplayers package
- **Background Processing**: workmanager
- **State Management**: Provider/Riverpod
- **Local Storage**: shared_preferences

## Roadmap

- [ ] iOS support with full sensor integration
- [ ] Remote monitoring via companion app
- [ ] GPS tracking integration
- [ ] Camera activation on detection
- [ ] Cloud recording of incidents
- [ ] Community sharing of dog breeds and barks
- [ ] Integration with smart car systems
- [ ] Multi-language support
- [ ] AI-powered threat detection

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/AmazingFeature`)
3. Commit your changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

## Privacy & Security

- All processing happens locally on your device
- No data is transmitted to external servers
- No tracking or analytics without your consent
- Open source for transparency

## Disclaimer

This app is intended as a supplementary security measure and should not replace proper car security systems. The effectiveness depends on phone placement, battery life, and sensor quality. Always secure your vehicle properly and never leave valuables visible.

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Support

For issues, questions, or suggestions:
- Open an issue on GitHub
- Contact: support@doggydogscaralarm.com

## Acknowledgments

- Dog bark sound effects from [source]
- Icon designs by [designer]
- Inspired by the need for creative, accessible car security solutions

---

Made with love for dogs and cars!
