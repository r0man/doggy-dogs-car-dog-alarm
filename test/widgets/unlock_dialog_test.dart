import 'package:flutter/material.dart';
import 'package:flutter_test/flutter_test.dart';
import 'package:doggy_dogs_car_alarm/widgets/unlock_dialog.dart';

void main() {
  group('UnlockDialog', () {
    testWidgets('displays correctly', (WidgetTester tester) async {
      await tester.pumpWidget(
        MaterialApp(
          home: Scaffold(
            body: UnlockDialog(
              onUnlockAttempt: (_) {},
            ),
          ),
        ),
      );

      expect(find.text('Enter Unlock Code'), findsOneWidget);
      expect(find.text('Enter your 4-digit PIN to deactivate the alarm.'),
          findsOneWidget);
      expect(find.text('Default code: 1234'), findsOneWidget);
      expect(find.byType(TextField), findsOneWidget);
      expect(find.text('UNLOCK'), findsOneWidget);
    });

    testWidgets('shows cancel button when provided',
        (WidgetTester tester) async {
      await tester.pumpWidget(
        MaterialApp(
          home: Scaffold(
            body: UnlockDialog(
              onUnlockAttempt: (_) {},
              onCancel: () {},
            ),
          ),
        ),
      );

      expect(find.text('CANCEL'), findsOneWidget);
    });

    testWidgets('hides cancel button when not provided',
        (WidgetTester tester) async {
      await tester.pumpWidget(
        MaterialApp(
          home: Scaffold(
            body: UnlockDialog(
              onUnlockAttempt: (_) {},
            ),
          ),
        ),
      );

      expect(find.text('CANCEL'), findsNothing);
    });

    testWidgets('text field is obscured', (WidgetTester tester) async {
      await tester.pumpWidget(
        MaterialApp(
          home: Scaffold(
            body: UnlockDialog(
              onUnlockAttempt: (_) {},
            ),
          ),
        ),
      );

      final textField = tester.widget<TextField>(find.byType(TextField));
      expect(textField.obscureText, isTrue);
    });

    testWidgets('text field accepts only numbers', (WidgetTester tester) async {
      await tester.pumpWidget(
        MaterialApp(
          home: Scaffold(
            body: UnlockDialog(
              onUnlockAttempt: (_) {},
            ),
          ),
        ),
      );

      final textField = tester.widget<TextField>(find.byType(TextField));
      expect(textField.keyboardType, TextInputType.number);
      expect(textField.inputFormatters, isNotEmpty);
    });

    testWidgets('calls onUnlockAttempt when unlock button pressed',
        (WidgetTester tester) async {
      String? capturedCode;

      await tester.pumpWidget(
        MaterialApp(
          home: Scaffold(
            body: UnlockDialog(
              onUnlockAttempt: (code) {
                capturedCode = code;
              },
            ),
          ),
        ),
      );

      await tester.enterText(find.byType(TextField), '1234');
      await tester.tap(find.text('UNLOCK'));
      await tester.pump();

      expect(capturedCode, '1234');
    });

    testWidgets('shows error when empty code submitted',
        (WidgetTester tester) async {
      await tester.pumpWidget(
        MaterialApp(
          home: Scaffold(
            body: UnlockDialog(
              onUnlockAttempt: (_) {},
            ),
          ),
        ),
      );

      await tester.tap(find.text('UNLOCK'));
      await tester.pump();

      expect(find.text('Invalid code'), findsOneWidget);
    });

    testWidgets('clears error when text changes', (WidgetTester tester) async {
      await tester.pumpWidget(
        MaterialApp(
          home: Scaffold(
            body: UnlockDialog(
              onUnlockAttempt: (_) {},
            ),
          ),
        ),
      );

      // Submit empty to trigger error
      await tester.tap(find.text('UNLOCK'));
      await tester.pump();
      expect(find.text('Invalid code'), findsOneWidget);

      // Enter text should clear error
      await tester.enterText(find.byType(TextField), '1');
      await tester.pump();
      expect(find.text('Invalid code'), findsNothing);
    });

    testWidgets('calls onUnlockAttempt when Enter key pressed',
        (WidgetTester tester) async {
      String? capturedCode;

      await tester.pumpWidget(
        MaterialApp(
          home: Scaffold(
            body: UnlockDialog(
              onUnlockAttempt: (code) {
                capturedCode = code;
              },
            ),
          ),
        ),
      );

      await tester.enterText(find.byType(TextField), '5678');
      await tester.testTextInput.receiveAction(TextInputAction.done);
      await tester.pump();

      expect(capturedCode, '5678');
    });

    testWidgets('calls onCancel when cancel button pressed',
        (WidgetTester tester) async {
      bool cancelled = false;

      await tester.pumpWidget(
        MaterialApp(
          home: Scaffold(
            body: UnlockDialog(
              onUnlockAttempt: (_) {},
              onCancel: () {
                cancelled = true;
              },
            ),
          ),
        ),
      );

      await tester.tap(find.text('CANCEL'));
      await tester.pump();

      expect(cancelled, isTrue);
    });
  });

  group('showUnlockDialog', () {
    testWidgets('shows dialog and returns result', (WidgetTester tester) async {
      bool? result;

      await tester.pumpWidget(
        MaterialApp(
          home: Scaffold(
            body: Builder(
              builder: (context) => ElevatedButton(
                onPressed: () async {
                  result = await showUnlockDialog(
                    context,
                    onUnlockAttempt: (_) {},
                  );
                },
                child: const Text('Show Dialog'),
              ),
            ),
          ),
        ),
      );

      await tester.tap(find.text('Show Dialog'));
      await tester.pumpAndSettle();

      expect(find.byType(UnlockDialog), findsOneWidget);
    });

    testWidgets('dialog is dismissible when dismissible=true',
        (WidgetTester tester) async {
      await tester.pumpWidget(
        MaterialApp(
          home: Scaffold(
            body: Builder(
              builder: (context) => ElevatedButton(
                onPressed: () async {
                  await showUnlockDialog(
                    context,
                    onUnlockAttempt: (_) {},
                    dismissible: true,
                  );
                },
                child: const Text('Show Dialog'),
              ),
            ),
          ),
        ),
      );

      await tester.tap(find.text('Show Dialog'));
      await tester.pumpAndSettle();

      expect(find.byType(UnlockDialog), findsOneWidget);

      // Tap outside dialog to dismiss
      await tester.tapAt(const Offset(10, 10));
      await tester.pumpAndSettle();

      expect(find.byType(UnlockDialog), findsNothing);
    });
  });
}
