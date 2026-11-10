# Reviewing ShenYu changes

Compare the actual final diff with the selected SPECs, Design and TASK scope.
Check behavior and compatibility across affected modules, SQL dialects and
upgrade paths. Review authorization, configuration updates, deadlines and
resource lifetimes where the change touches them.

Confirm verification belongs to the current candidate. Separate regression
tests from compilation or skipped tests, and assess important coverage gaps.
Check that the PR title/body explain the final result and that issue-closing
references match what the PR actually resolves.

The policy check establishes metadata and planning consistency only. Maintainers
assess whether the declared scope is truthful and whether the code satisfies
the requirement. Keep review/approval and merge under existing project policy.
Do not require a separate agent, a REVIEW/VERIFY artifact or automatic merge
because IssueSpec is configured.
