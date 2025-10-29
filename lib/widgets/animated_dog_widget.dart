import 'package:flutter/material.dart';
import 'package:flutter_svg/flutter_svg.dart';
import '../models/dog.dart';
import '../models/dog_animation_state.dart';
import '../services/dog_animation_controller.dart';

/// Displays an animated virtual dog with smooth state transitions
class AnimatedDogWidget extends StatefulWidget {
  final DogBreed breed;
  final DogAnimationController controller;
  final double size;

  const AnimatedDogWidget({
    super.key,
    required this.breed,
    required this.controller,
    this.size = 200.0,
  });

  @override
  State<AnimatedDogWidget> createState() => _AnimatedDogWidgetState();
}

class _AnimatedDogWidgetState extends State<AnimatedDogWidget>
    with SingleTickerProviderStateMixin {
  late AnimationController _animController;
  late Animation<double> _scaleAnimation;
  late Animation<double> _opacityAnimation;
  late Animation<double> _rotationAnimation;

  @override
  void initState() {
    super.initState();
    _setupAnimations();
    widget.controller.addListener(_onAnimationStateChanged);
  }

  @override
  void dispose() {
    widget.controller.removeListener(_onAnimationStateChanged);
    _animController.dispose();
    super.dispose();
  }

  void _setupAnimations() {
    _animController = AnimationController(
      vsync: this,
      duration: const Duration(milliseconds: 400),
    );

    _scaleAnimation = Tween<double>(begin: 1.0, end: 1.0).animate(
      CurvedAnimation(parent: _animController, curve: Curves.easeInOut),
    );

    _opacityAnimation = Tween<double>(begin: 1.0, end: 1.0).animate(
      CurvedAnimation(parent: _animController, curve: Curves.easeInOut),
    );

    _rotationAnimation = Tween<double>(begin: 0.0, end: 0.0).animate(
      CurvedAnimation(parent: _animController, curve: Curves.easeInOut),
    );
  }

  void _onAnimationStateChanged() {
    setState(() {
      _updateAnimationForState(widget.controller.currentState);
    });
  }

  void _updateAnimationForState(DogAnimationState state) {
    // Reset animations
    _animController.reset();

    // Configure animations based on state
    switch (state) {
      case DogAnimationState.idle:
        _scaleAnimation = Tween<double>(begin: 1.0, end: 1.02).animate(
          CurvedAnimation(parent: _animController, curve: Curves.easeInOut),
        );
        _animController.duration = const Duration(seconds: 2);
        _animController.repeat(reverse: true); // Gentle breathing

      case DogAnimationState.alert:
        _scaleAnimation = Tween<double>(begin: 1.0, end: 1.05).animate(
          CurvedAnimation(parent: _animController, curve: Curves.easeOut),
        );
        _animController.duration = const Duration(milliseconds: 300);
        _animController.forward(); // Quick scale up, stay alert

      case DogAnimationState.barking:
        _scaleAnimation = Tween<double>(begin: 1.0, end: 1.1).animate(
          CurvedAnimation(parent: _animController, curve: Curves.elasticOut),
        );
        _rotationAnimation = Tween<double>(begin: -0.05, end: 0.05).animate(
          CurvedAnimation(parent: _animController, curve: Curves.easeInOut),
        );
        _animController.duration = const Duration(milliseconds: 500);
        _animController.repeat(reverse: true); // Bouncy barking

      case DogAnimationState.happy:
        _scaleAnimation = Tween<double>(begin: 1.0, end: 1.08).animate(
          CurvedAnimation(parent: _animController, curve: Curves.bounceOut),
        );
        _rotationAnimation = Tween<double>(begin: -0.1, end: 0.1).animate(
          CurvedAnimation(parent: _animController, curve: Curves.easeInOut),
        );
        _animController.duration = const Duration(milliseconds: 600);
        _animController.repeat(reverse: true); // Happy wiggle

      case DogAnimationState.sad:
        _scaleAnimation = Tween<double>(begin: 1.0, end: 0.95).animate(
          CurvedAnimation(parent: _animController, curve: Curves.easeOut),
        );
        _opacityAnimation = Tween<double>(begin: 1.0, end: 0.8).animate(
          CurvedAnimation(parent: _animController, curve: Curves.easeOut),
        );
        _animController.duration = const Duration(milliseconds: 800);
        _animController.forward(); // Shrink and dim

      case DogAnimationState.sleeping:
        _scaleAnimation = Tween<double>(begin: 1.0, end: 1.01).animate(
          CurvedAnimation(parent: _animController, curve: Curves.easeInOut),
        );
        _opacityAnimation = Tween<double>(begin: 1.0, end: 0.9).animate(
          CurvedAnimation(parent: _animController, curve: Curves.easeInOut),
        );
        _animController.duration = const Duration(seconds: 3);
        _animController.repeat(reverse: true); // Slow breathing

      case DogAnimationState.eating:
        _rotationAnimation = Tween<double>(begin: 0.0, end: 0.15).animate(
          CurvedAnimation(parent: _animController, curve: Curves.easeInOut),
        );
        _animController.duration = const Duration(milliseconds: 400);
        _animController.repeat(
            reverse: true, period: const Duration(seconds: 3));

      case DogAnimationState.playing:
        _scaleAnimation = Tween<double>(begin: 1.0, end: 1.15).animate(
          CurvedAnimation(parent: _animController, curve: Curves.elasticOut),
        );
        _rotationAnimation = Tween<double>(begin: -0.15, end: 0.15).animate(
          CurvedAnimation(parent: _animController, curve: Curves.easeInOut),
        );
        _animController.duration = const Duration(milliseconds: 400);
        _animController.repeat(
            reverse: true, period: const Duration(milliseconds: 2500));
    }
  }

  @override
  Widget build(BuildContext context) {
    return SizedBox(
      width: widget.size,
      height: widget.size,
      child: AnimatedBuilder(
        animation: _animController,
        builder: (context, child) {
          return Transform.scale(
            scale: _scaleAnimation.value,
            child: Transform.rotate(
              angle: _rotationAnimation.value,
              child: Opacity(
                opacity: _opacityAnimation.value,
                child: _buildDogImage(),
              ),
            ),
          );
        },
      ),
    );
  }

  Widget _buildDogImage() {
    // TODO: Support Rive animations when assets are available
    // For now, use SVG with transforms as placeholder

    return SvgPicture.asset(
      widget.breed.assetPath,
      width: widget.size,
      height: widget.size,
      fit: BoxFit.contain,
      placeholderBuilder: (context) => Container(
        width: widget.size,
        height: widget.size,
        color: Colors.grey[300],
        child: const Center(
          child: CircularProgressIndicator(),
        ),
      ),
    );
  }
}
