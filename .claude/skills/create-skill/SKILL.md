---
name: create-skill
description: Create a new Claude Code skill (custom slash command). Use when the user wants to add a new skill or slash command to the project.
argument-hint: [skill-name]
---

# Create Claude Code Skill

Create a new skill named: $ARGUMENTS

## Skill Structure

Skills are directories containing a `SKILL.md` file:

```
.claude/skills/<skill-name>/
├── SKILL.md           # Main instructions (required)
├── templates/         # Optional templates
├── examples/          # Optional examples
└── scripts/           # Optional scripts Claude can execute
```

## SKILL.md Format

Every skill needs YAML frontmatter and markdown content:

```yaml
---
name: skill-name
description: What this skill does and when to use it
argument-hint: [optional-args]
---

# Skill Title

Instructions for Claude when this skill is invoked.

Use $ARGUMENTS to access arguments passed to the skill.
Use $ARGUMENTS[0], $ARGUMENTS[1] or $0, $1 for positional args.
```

## Frontmatter Fields

| Field | Required | Description |
|-------|----------|-------------|
| `name` | No | Display name (defaults to directory name). Lowercase, numbers, hyphens only. |
| `description` | Recommended | What the skill does. Claude uses this to decide when to load it. |
| `argument-hint` | No | Hint shown during autocomplete, e.g., `[filename]` or `[issue-number]` |
| `disable-model-invocation` | No | Set `true` to prevent Claude from auto-invoking (manual `/name` only) |
| `user-invocable` | No | Set `false` to hide from `/` menu (Claude-only background knowledge) |
| `allowed-tools` | No | Tools Claude can use without permission when skill is active |
| `context` | No | Set `fork` to run in isolated subagent context |
| `agent` | No | Subagent type when `context: fork` (`Explore`, `Plan`, `general-purpose`) |

## Storage Locations

| Location | Path | Scope |
|----------|------|-------|
| Personal | `~/.claude/skills/<name>/SKILL.md` | All your projects |
| Project | `.claude/skills/<name>/SKILL.md` | This project only |

## Variable Substitutions

| Variable | Description |
|----------|-------------|
| `$ARGUMENTS` | All arguments passed when invoking |
| `$ARGUMENTS[N]` or `$N` | Specific argument by index (0-based) |
| `${CLAUDE_SESSION_ID}` | Current session ID |

## Dynamic Context

Run shell commands before skill content is sent to Claude:

```markdown
Current branch: !`git branch --show-current`
Recent commits: !`git log --oneline -5`
```

The command output replaces the placeholder.

## Examples

### Simple Reference Skill

```yaml
---
name: api-conventions
description: API design patterns for this codebase
---

When writing API endpoints:
- Use RESTful naming conventions
- Return consistent error formats
- Include request validation
```

### Task Skill (Manual Only)

```yaml
---
name: deploy
description: Deploy the application to production
disable-model-invocation: true
---

Deploy $ARGUMENTS to production:
1. Run the test suite
2. Build the application
3. Push to deployment target
```

### Skill with Subagent

```yaml
---
name: deep-research
description: Research a topic thoroughly
context: fork
agent: Explore
---

Research $ARGUMENTS thoroughly:
1. Find relevant files using Glob and Grep
2. Read and analyze the code
3. Summarize findings with file references
```

## Invocation Control

| Frontmatter | You invoke | Claude invokes | Use case |
|-------------|------------|----------------|----------|
| (default) | Yes | Yes | General purpose skills |
| `disable-model-invocation: true` | Yes | No | Actions with side effects (deploy, commit) |
| `user-invocable: false` | No | Yes | Background knowledge |

## Best Practices

1. Keep `SKILL.md` under 500 lines; use supporting files for details
2. Write clear descriptions so Claude knows when to use the skill
3. Use `disable-model-invocation: true` for destructive or side-effect actions
4. Test with both direct invocation (`/skill-name`) and natural language
5. Reference supporting files from SKILL.md so Claude knows when to load them
