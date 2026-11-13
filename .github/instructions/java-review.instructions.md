---
applyTo: "**/src/main/java/**/*.java"
excludeAgent: "coding-agent"
---

# Java code review rules for Apache ShenYu

Review only lines changed by the pull request. Rule IDs Q1-Q9 are defined in `AGENTS.md`.

## Do not repeat CI

Checkstyle (`script/shenyu_checkstyle.xml`) enforces formatting, imports, Javadoc, final parameters and license headers. The `new-code-quality` job reports Q1-Q6 on added lines. Do not comment on findings those tools already report.

## Check what tools miss

- Q1: named constants without meaning or unit (`NUM`, `VALUE_3`); numbers inside strings or durations (`"3000"`, `Duration.ofMillis(3000)`); a new constant that duplicates an existing one in `Constants` or the module.
- Q7: re-implemented helpers that already exist (`GsonUtils`, `JsonUtils`, `UriUtils`, `MapUtils`, `ListUtil`, plugin base classes, SPI loaders).
- Q8: commented-out code, unused members or parameters, speculative options or extension points.
- Q9: names that hide intent, methods doing several things, comments restating code.
- Reactive code: blocking calls on event-loop threads, `DataBuffer` not released, errors swallowed, subscriptions not cancelled with the request.
- Compatibility: changed public SPI, plugin names, configuration keys or defaults without a stated reason.
- Secrets: credentials or tokens in logs or exceptions.

## Comment format

- One finding per comment, on the relevant line.
- Start with a severity and rule, for example `[blocking][Q7]` or `[suggestion][Q1]`. Use `blocking` only for correctness, compatibility, security or resource leaks.
- State the problem, why it matters here, and a concrete fix.
- Say "verified" when you traced the code, otherwise "suspected".
- At most 10 comments. No praise, no summary of the change.

## Guarded files

If the pull request changes `AGENTS.md`, `.github/copilot-instructions.md`, `.github/instructions/**`, `script/shenyu_checkstyle*.xml` or `.github/scripts/check-new-code-quality.py`, point it out explicitly: these define the rules for authors and reviewers.
