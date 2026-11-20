<!-- Describe your PR here; e.g. Fixes #issueNo -->

## Problem and change

<!-- Explain the trigger and resulting behavior, material choices, compatibility
     and limitations. Link issues and any required unmerged PRs. Keep small
     changes brief. Use Fixes only when the issue is fully resolved. -->

## IssueSpec

<!-- Choose full or lightweight. Keep the metadata below as plain lines.
     See issue-spec/project/workflow.md for examples and maintainer classification.
     For lightweight, remove Proposal/Design/Implement/Specs/Tasks.
     For full, replace every reference and ID with the actual selected artifacts. -->
IssueSpec-Mode: full | lightweight
IssueSpec-Reason: describe the scope and why this mode applies
IssueSpec-Proposal: #NUMBER
IssueSpec-Design: #NUMBER
IssueSpec-Implement: #NUMBER
IssueSpec-Specs: SPEC-ID
IssueSpec-Tasks: TASK-ID
IssueSpec-Validation: describe checks, results and important checks not run

<!--
Thank you for proposing a pull request. This template will guide you through the essential steps necessary for a pull request.
-->
Make sure that:

- [ ] You have read the [contribution guidelines](https://shenyu.apache.org/community/contributor-guide).
- [ ] You submit test cases (unit or integration tests) that back your changes.
- [ ] New production code follows the code quality rules Q1-Q9 in `AGENTS.md` (no magic numbers, focused methods, reuse of existing utilities).
- [ ] Your local test passed `./mvnw clean install -Dmaven.javadoc.skip=true`.
