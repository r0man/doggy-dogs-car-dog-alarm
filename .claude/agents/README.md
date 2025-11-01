# Claude Agents for Doggy Dogs Car Alarm

This directory contains specialized Claude agent definitions that provide expert assistance for different aspects of the project.

## Available Agents

### 🔧 Flutter Expert (`flutter-expert.md`)
**When to use**: For implementing features, architectural decisions, and Flutter best practices.

**Expertise**:
- Flutter 3.x & Dart 3.x development
- Riverpod state management
- Clean architecture patterns
- Performance optimization
- Platform integrations

**Example prompts**:
- "Help me implement dog training mini-games"
- "How should I structure the geofencing feature?"
- "Review this state management approach for the alarm history"

### 🔍 Code Reviewer (`code-reviewer.md`)
**When to use**: For code review, quality checks, and identifying issues before merge.

**Focus areas**:
- Security vulnerabilities
- Performance problems
- Architecture violations
- Testing gaps
- Best practices compliance

**Example prompts**:
- "Review the changes in PR #42"
- "Check this alarm service implementation for issues"
- "Is this widget properly optimized?"

### 🧪 Code Tester (`code-tester.md`)
**When to use**: For test strategy, writing tests, and improving test coverage.

**Expertise**:
- Unit tests (models, services, utilities)
- Widget tests (UI components, interactions)
- Integration tests (user flows)
- Test coverage analysis (target: ≥85%)
- Mocking and test data strategies

**Example prompts**:
- "Write tests for the new achievement system"
- "How should I test the background monitoring service?"
- "Review test coverage for the sensor detection module"

### 📋 Product Manager (`product-manager.md`)
**When to use**: For requirements clarification, feature prioritization, and product decisions.

**Focus areas**:
- User stories and acceptance criteria
- Feature prioritization (P0-P3)
- Metrics and success measures
- Product roadmap alignment
- Scope definition

**Example prompts**:
- "Help me define requirements for the social sharing feature"
- "Should we prioritize achievements or alarm analytics?"
- "What acceptance criteria should Issue #8 have?"

## How to Use These Agents

### In Claude Code CLI

While these agent definitions are stored as markdown files for reference, you can invoke specialized behavior by:

1. **Reference the agent**: "Acting as the Flutter Expert agent..."
2. **Specify the role**: "Review this code as the Code Reviewer would..."
3. **Request analysis**: "From a Product Manager perspective, should we..."

### Agent Interaction Patterns

#### For Implementation
```
User: "I need to implement alarm history (Issue #15)"

Assistant (as Flutter Expert):
1. Explains architecture approach
2. Lists files to create/modify
3. Shows implementation with code samples
4. Describes testing strategy
5. Notes any caveats
```

#### For Code Review
```
User: "Review this AlarmHistoryService implementation"

Assistant (as Code Reviewer):
1. Summary of changes
2. Critical issues (must fix)
3. Important issues (should fix)
4. Minor suggestions
5. Positive feedback
6. Overall recommendation (approve/changes needed)
```

#### For Testing
```
User: "Write tests for the achievement system"

Assistant (as Code Tester):
1. Test strategy (what to test, how)
2. Test categories (unit/widget/integration)
3. Complete test implementation
4. Coverage analysis
5. Run instructions
```

#### For Product Decisions
```
User: "Should we add a premium subscription?"

Assistant (as Product Manager):
1. Context (market fit, user needs)
2. Problem/solution analysis
3. Success metrics
4. Priority assessment (P0/P1/P2/P3)
5. Dependencies and constraints
6. Recommendation
```

## Agent Characteristics

### Flutter Expert
- **Tone**: Technical, precise, practical
- **Depth**: Implementation-level detail
- **Output**: Code samples, architectural diagrams, file lists
- **Asks**: Clarifies requirements, discusses trade-offs

### Code Reviewer
- **Tone**: Constructive, thorough, educational
- **Depth**: Line-by-line analysis when needed
- **Output**: Structured feedback, suggestions, examples
- **Asks**: About intent, design decisions, constraints

### Code Tester
- **Tone**: Methodical, comprehensive, quality-focused
- **Depth**: Test case level detail
- **Output**: Test code, coverage reports, strategies
- **Asks**: About edge cases, expected behavior, test scope

### Product Manager
- **Tone**: User-focused, strategic, balanced
- **Depth**: Feature-level, not implementation
- **Output**: User stories, acceptance criteria, priorities
- **Asks**: About user problems, business goals, metrics

## Project Context

All agents are aware of:

- **Product**: Doggy Dogs Car Dog Alarm - virtual pet guard dog for car security
- **Vision**: Transform car security into an emotionally engaging experience
- **Tech Stack**: Flutter 3.35.0, Riverpod, sensors_plus, workmanager
- **Architecture**: Clean architecture, feature-based organization
- **Quality Bar**: ≥85% test coverage, all tests passing
- **Current State**: MVP complete, working on engagement features

## Combining Agents

You can involve multiple agents for complex tasks:

```
User: "I want to add a social feature to share dog achievements"

1. Product Manager: Define requirements, acceptance criteria, priority
2. Flutter Expert: Design architecture, recommend implementation approach
3. Code Tester: Plan test strategy, identify test scenarios
4. Code Reviewer: Review implementation, ensure quality standards
```

## Updating Agents

These agent definitions should evolve as the project grows:

- Add new project patterns and conventions
- Update tech stack changes
- Reflect architectural decisions
- Incorporate lessons learned
- Expand domain knowledge

To update: Edit the markdown files and commit with clear changelog.

## Tips for Best Results

1. **Be specific**: "Review the alarm persistence logic" vs "review the code"
2. **Provide context**: Link to issues, PRs, or paste relevant code
3. **State your goal**: "I want to optimize performance" vs "make this better"
4. **Ask follow-ups**: Agents can explain reasoning, suggest alternatives
5. **Combine perspectives**: Use multiple agents for comprehensive analysis

---

These agents are designed to help maintain high code quality, clear requirements, and effective collaboration throughout the project lifecycle.
