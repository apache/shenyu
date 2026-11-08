# Plugin graph runtime: initial implementation for #7996

Status: development draft, not a completed #7996 implementation or a production activation path.

## Baseline and dependency alignment

Checked on 2026-11-08:

- Target branch: Apache `docs/plugin-graph-contract` at `4ded4c4f1`
  (2.7.2-SNAPSHOT), which contains the #8008 model/compiler implementation.
- #8007 merged into master on 2026-10-26 (`b46bf15c3`), but #8008 merged into
  `docs/plugin-graph-contract`, **not master**.
- `feat/7996-graph-runtime` is based directly on that target branch. The runtime
  contribution contains no copied model/compiler commit or unrelated master changes.
  Review it against `docs/plugin-graph-contract`; retarget to master only after the
  dependency lands there and the runtime has been rebased and revalidated.
- #7996 is assigned to SouthwestAsiaFloat. #8015 is still open. Merging the design
  artifact did not resolve the five distinct continuation cases.

The model uses `GraphNodeData.next` and `GraphBranchData.matchMode` (0 AND, 1 OR).
The binder uses those actual fields, not the indicative `match: all` JSON example.

## Implemented seam

`ShenyuWebHandler` retains its existing three-argument constructor and LEGACY behavior.
An additional constructor accepts a `PluginExecutionDispatcher` for explicit graph
execution in tests or development wiring. No Spring property enables GRAPH yet.

The dispatcher selects before plugin execution and before scheduler queueing.
The selected GRAPH provider is queried once inside the handler's entry subscription.
A missing or mismatched namespace/graph/deployment binding produces 503; execution
errors propagate to existing error handling without restarting LEGACY. A dispatcher
has a fixed mode; swapping the provider's plan cannot switch modes.

`GraphPlanBinder` runs the existing structural compiler and resolves supplied plugin
instances. It creates an internal topology snapshot, copying node/branch/condition
values because the upstream `GraphPlan` currently holds mutable node DTOs.
It validates match modes and condition SPI names instead of silently defaulting an
unknown mode to AND. Conditions use the existing MatchStrategy/ParameterData/PredicateJudge
machinery on the current, possibly decorated exchange.

`GraphExecutor` creates a `GraphExecution` for each entry subscription.
PLUGIN nodes retain skip, selector/rule matching, construction-time before/after,
short circuits and nested publishers. CHOICE selects the first matching branch or its
default; predicate errors propagate. END returns an empty completion, leaving response
completion to the plugin that owns it. No internal subscribe, block, cache or parallel
scheduler is introduced.

## Provisional continuation policy for review (#8015)

This implementation deliberately tests a **candidate**, not an accepted contract:
one ordinary continuation may start its successor once per plugin invocation.

| Case | Current draft behavior |
| --- | --- |
| Two calls to the same continuation | Both return cold publishers; only the first subscribed execution may start the successor. The other errors. |
| Two subscriptions to the same successor publisher | The second errors, even after cancellation or failure; side effects are not implicitly replayed. |
| HTTP transport-internal retry | Unaffected when retry surrounds the transport publisher and continuation is invoked after a response. |
| Repeated graph entry subscription | Fresh execution and continuation state, same pinned plan for a direct executor entry; the handler consults its provider again for a new request subscription. |
| Explicit fork | Fresh entry execution, same plan/executor, child exchange supplied by caller, bounded recursion depth. |

The guard is local to a plugin invocation and is checked at subscription, not publisher
construction. A fork does not consume its parent's ordinary continuation.
The pending contract proposal #8344 calls for separate atomic invocation and subscription
guards. This draft has only the subscription guard; that alignment remains part of #8015.

The candidate intentionally differs from the mutable legacy cursor on repeated
continuations. Do not advertise general retry/fallback compatibility from these tests.
No supported successor retry attempt exists in this candidate, so CHOICE is not replayed
through an ordinary continuation. Fresh roots and forks evaluate CHOICE on their own
exchange. Transport retries do not rerun CHOICE outside the transport publisher.

An entry re-subscription creates state; it does **not** make a consumed HTTP request
body or an already-committed response safe to reuse.

## Remaining acceptance work before production configuration

1. Resolve #8015 with the contract owners and audit concrete supported plugins.
   The current transport test is a behavioral fixture, not a real HTTP client integration.
   MCP stores continuations for later callbacks; that lifecycle is not supported by this
   one-shot draft. Do not admit it as compatible without an adapter or reviewed restriction.
2. Finish semantic admission: a structurally valid plan alone does not guarantee one
   global/auth/proxy/response pipeline or valid wrapper scope (#8010, #8013, #8014).
   Missing selectors/rules still follow the plugin's existing semantics; `required`
   does not mean authentication succeeded.
3. Connect an admitted-plan provider to the authoritative enabled plugin catalog,
   atomic publication, explicit-disable invalidation, loader leases and retirement
   (#7999 / #8009). The binder takes a catalog snapshot but is not a second catalog.
   Existing executions/forks require resource lifetime guarantees beyond Java references.
4. Add deployment-level configuration (default LEGACY), readiness and graph binding
   once admission/lifecycle are available. Plan synchronization remains #7998.
5. Add real response-writer, HTTP transport, resilience, decorated request-body and
   long-lived stream integration coverage, plus condition-error and concurrent execution cases.

The runtime source does not modify the old cursor, legacy sorting, the compiler DTOs,
the sync protocol or the Spring bootstrap configuration. This boundary is intentional
while the acceptance items above remain unresolved.

## Validation

Use Java 17 and the repository wrapper:

```sh
./mvnw -B -ntp -pl shenyu-web -am clean test \
  '-Dtest=org/apache/shenyu/web/**/*Test,GraphCompilerTest,GraphValidatorTest,GraphTopologyTest,LegacyChainImporterTest' \
  -Dsurefire.failIfNoSpecifiedTests=false \
  -Djacoco.skip=true -Dmaven.javadoc.skip=true -DskipRemoteResources=true
```

The focused suite covers legacy hook/wrapper parity, frozen topology/conditions,
decorated exchange selection, first-match/default/convergence, skip and short circuits,
error recovery, cancellation, response ownership at END, the five continuation cases,
fork limits, no legacy fallback, binding rejection, and real AbstractShenyuPlugin
selector/rule behavior. Existing handler lifecycle tests retain entry/scheduler coverage.

Validation on 2026-11-08 with Java 17 against `docs/plugin-graph-contract`:

- Clean reactor build and full shenyu-web regression: 129 tests, including all 15 graph
  runtime tests, zero failures/errors/skips.
- GraphCompiler/GraphValidator/GraphTopology/LegacyChainImporter: 44 tests, zero failures/errors.
- Checkstyle passed for the reactor modules; shenyu-web Apache RAT reported zero unapproved licenses.
- Full-repository install and Docker integration/e2e suites were not run.

RAT was run with `./mvnw -B -ntp -pl shenyu-web apache-rat:check`.
