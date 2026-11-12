# ShenYu IssueSpec policy

This policy adds explicit planning for changes to behavior and public contracts.
The initial GitHub check is **advisory**: findings produce a neutral check, not
merge authorization. Existing tests, reviews and contribution requirements apply.

## Choose the scope

Use **full** for new capabilities or changes to public SPI, configuration
semantics, authorization, persistence/upgrade behavior or Admin-to-gateway
contracts. A full change has linked Proposal, Design and Implement issues, at
least one confirmed SPEC and an implementation TASK covering every SPEC selected
by the PR. Several focused PRs may implement one change; each names its own
SPEC/TASK subset. PROCESS is optional and does not require delegation.

Use **lightweight** for documentation, tests and fixes preserving the existing
contract. Describe the reason and verification. The checker automatically
recognizes ordinary documentation and `src/test/` changes. It conservatively
treats production, build, CI and agent/workflow instructions as requiring full
planning until a maintainer classifies a behavior-preserving change.

A maintainer with current write, maintain or admin permission may post a PR
comment containing `IssueSpec-Classification: lightweight <40-character-head-SHA>`
to classify such a change. The newest authorized comment for that exact head
wins; `IssueSpec-Classification: full <SHA>` revokes the exception. A new commit
needs renewed classification. Editing/deleting these comments triggers another
check, and missing history or permission evidence grants no exception.
The PR still needs a substantive reason and validation description.
The maintainer-managed `issuespec:required` label takes precedence.
CI never creates labels or edits issues. A new feature cannot qualify merely
by being small. Classification is scoped planning advice, not review approval.

The path classifier is conservative routing, not a semantic proof. Reviewers
check whether documentation or tests actually change a public contract and
require full planning when appropriate. Rename checks include both old and new
paths.

## PR metadata

Keep these fields as visible, unquoted lines outside code blocks and HTML
comments. Duplicate or unknown fields are errors. References may be `#123` or
exact `https://github.com/apache/shenyu/issues/123` URLs. The issue numbers below
are illustrative, not existing approved plans.

~~~text
IssueSpec-Mode: full
IssueSpec-Reason: Add resource permissions to the built-in identity adapters.
IssueSpec-Proposal: #1001
IssueSpec-Design: #1002
IssueSpec-Implement: #1003
IssueSpec-Specs: SPEC-1001001
IssueSpec-Tasks: TASK-1002001
IssueSpec-Validation: List the actual commands, results and important unrun checks.
~~~

For a lightweight change omit the five artifact-reference fields:

~~~text
IssueSpec-Mode: lightweight
IssueSpec-Reason: Correct a documentation link without changing the described behavior.
IssueSpec-Validation: Checked the target document and relative links; runtime tests not applicable.
~~~

The metadata is a ShenYu convention, not a new upstream IssueSpec command.
It states validation evidence for reviewers; the policy check cannot establish
that a claimed test ran or that a linked requirement describes the actual diff.
Never mark an existing PR template checkbox for a check that was not run.

Use a concrete PR title. For example, a bugfix description can state the trigger,
before/after behavior and the regression test actually executed. A documentation
PR can use one sentence describing the correction and its link/content checks.
Rewrite the title and body after scope changes rather than retaining abandoned
approaches. Link partial work with `Related to` and use `Fixes` only on completion.

## Tool and workflow ownership

The checked-in workflow pins upstream IssueSpec to
`ab3e3944b052cc353ab4983c4a8a09e5e09fc2dc`. Install this revision for compatible
local authoring:

The CLI requires Go 1.25 or newer. Its
[pinned source and authoring documentation](https://github.com/higress-group/issue-spec/tree/ab3e3944b052cc353ab4983c4a8a09e5e09fc2dc)
define the phase issues and typed comment formats.

~~~bash
go install github.com/higress-group/issue-spec/cmd/issue-spec@ab3e3944b052cc353ab4983c4a8a09e5e09fc2dc
issue-spec workflow validate --repo apache/shenyu --json
~~~

Before opening a full PR, check its linked plan with
`issue-spec status --repo apache/shenyu --proposal <number> --design <number> --implement <number> --gate implement --json`.
Each selected SPEC must be confirmed and carry a durable intent; each selected
TASK must be ready, in-progress or done and link to the SPEC's canonical comment
URL. A successful upstream status alone does not replace the PR's selected IDs.

Optional `issue-spec init --repo apache/shenyu --tools codex,claude --skip-labels`
installs generated skills. Review generated files before committing them.
Project-owned rules live here and in `AGENTS.md`; inspect
`workflow update --dry-run` before upgrading generated files. Installing the
CLI or selecting this workflow does not authorize publication, delegation,
approval or merge beyond the user's actual request.

At this revision `status` checks planning consistency, not delivery acceptance.
The old `review`, `verify` and `archive` writers are retired. Use project tests,
provider-native review and `durable-spec preview/apply/check` for their respective
purposes. Upstream documentation contains differing role guidance; ShenYu's
current agent may implement and self-test directly. Add delegation only when
authorized and useful.

## Advisory rollout and operations

`.github/workflows/issue-spec-policy.yml` reads scripts, configuration and the
pinned tool from the trusted default branch. It never checks out or executes
the candidate PR. Pull-request changes and linked issue/comment changes
re-evaluate the current head; manual dispatch accepts a PR number. Each result
records the head SHA and a digest of the observed issue bodies and comments.
If those inputs change during validation, the result is inconclusive and must
be rerun. API/tool errors also remain explicit rather than passing as valid.

The check validates phase identity/predecessors, selected artifact presence and
TASK coverage in addition to upstream canonical/gate validation. It does not
require historical REVIEW/VERIFY objects, PROCESSes or a new approval receipt.
Durable-spec content belongs in the implementation PR and is reviewed with its
tests; the privileged policy job never applies or evaluates candidate code.

After this workflow lands on the default branch, exercise a same-repository PR,
a fork PR, documentation-only PR, a maintainer-classified fix and a full change.
Also test PR edits/pushes, removed or malformed SPECs, unresolved questions,
label removal, missing permissions and unavailable APIs. Existing PRs receive
advice during this observation period; they are not silently exempted later.
Checks triggered by GitHub's own workflow token may need manual dispatch because
GitHub suppresses some recursive events. No automatic merge-time atomicity with
mutable issue content is claimed.

Only after reviewing that evidence should a separate reviewed change set
`POLICY_MODE` to `enforce` and add `issue-spec-policy` to
`github.protected_branches.master.required_status_checks.contexts` in
`.asf.yaml`, preserving existing required checks. Confirm ASF synchronization
and the actual GitHub check name/source. Do not add a required context before
every PR can receive a result. Restore observe mode and remove the required
context together if rollback is needed.

Local checks:

~~~bash
node --test .github/scripts/issue-spec-policy-test.mjs
node --test .github/scripts/test-ci-workflows.mjs
issue-spec workflow validate --repo apache/shenyu --json
~~~
