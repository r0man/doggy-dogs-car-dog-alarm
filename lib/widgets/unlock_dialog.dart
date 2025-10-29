import 'package:flutter/material.dart';
import 'package:flutter/services.dart';

/// Dialog for entering unlock code to deactivate alarm
class UnlockDialog extends StatefulWidget {
  final Function(String) onUnlockAttempt;
  final VoidCallback? onCancel;

  const UnlockDialog({
    super.key,
    required this.onUnlockAttempt,
    this.onCancel,
  });

  @override
  State<UnlockDialog> createState() => _UnlockDialogState();
}

class _UnlockDialogState extends State<UnlockDialog> {
  final TextEditingController _controller = TextEditingController();
  bool _isError = false;

  @override
  void dispose() {
    _controller.dispose();
    super.dispose();
  }

  void _submitCode() {
    final code = _controller.text.trim();
    if (code.isEmpty) {
      setState(() => _isError = true);
      return;
    }

    widget.onUnlockAttempt(code);
  }

  @override
  Widget build(BuildContext context) {
    return AlertDialog(
      title: const Row(
        children: [
          Icon(Icons.lock, color: Colors.orange),
          SizedBox(width: 8),
          Text('Enter Unlock Code'),
        ],
      ),
      content: Column(
        mainAxisSize: MainAxisSize.min,
        crossAxisAlignment: CrossAxisAlignment.stretch,
        children: [
          const Text(
            'Enter your 4-digit PIN to deactivate the alarm.',
            style: TextStyle(fontSize: 14),
          ),
          const SizedBox(height: 16),
          TextField(
            controller: _controller,
            autofocus: true,
            obscureText: true,
            keyboardType: TextInputType.number,
            maxLength: 6,
            inputFormatters: [FilteringTextInputFormatter.digitsOnly],
            decoration: InputDecoration(
              labelText: 'Unlock Code',
              prefixIcon: const Icon(Icons.pin),
              border: const OutlineInputBorder(),
              errorText: _isError ? 'Invalid code' : null,
              counterText: '',
            ),
            onChanged: (_) {
              if (_isError) {
                setState(() => _isError = false);
              }
            },
            onSubmitted: (_) => _submitCode(),
          ),
          const SizedBox(height: 8),
          Text(
            'Default code: 1234',
            style: TextStyle(
              fontSize: 12,
              color: Colors.grey.shade600,
              fontStyle: FontStyle.italic,
            ),
          ),
        ],
      ),
      actions: [
        if (widget.onCancel != null)
          TextButton(
            onPressed: widget.onCancel,
            child: const Text('CANCEL'),
          ),
        ElevatedButton(
          onPressed: _submitCode,
          style: ElevatedButton.styleFrom(
            backgroundColor: Colors.green,
            foregroundColor: Colors.white,
          ),
          child: const Text('UNLOCK'),
        ),
      ],
    );
  }
}

/// Show unlock dialog and return true if successful
Future<bool> showUnlockDialog(
  BuildContext context, {
  required Function(String) onUnlockAttempt,
  bool dismissible = true,
}) async {
  final result = await showDialog<bool>(
    context: context,
    barrierDismissible: dismissible,
    builder: (context) => UnlockDialog(
      onUnlockAttempt: (code) {
        onUnlockAttempt(code);
      },
      onCancel: dismissible ? () => Navigator.of(context).pop(false) : null,
    ),
  );

  return result ?? false;
}
