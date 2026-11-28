# Java client migration disposition manifest

Planning slice of [#8423](https://github.com/apache/shenyu/issues/8423): the per-module, resource and test disposition required before any staged source is deleted. Reconciled against master `2fd9ccb09`; the inventory at `.github/client-migration-inventory.json` is byte-identical to its baseline `9dd9a12fb` and all 30 inventoried roots still exist, so no entry below needed a drift correction. Source removal itself stays gated by the evidence listed in the issue; this document only records what will be removed, what stays, and the order in which the removal keeps every intermediate tree buildable.

## Disposition

| Area | Paths | Disposition | Notes |
| --- | --- | --- | --- |
| Client implementations | `shenyu-client` (12 artifacts incl. aggregator, 189 files) | remove | External owner: apache/shenyu-client-java |
| Registration API/HTTP | `shenyu-register-center/shenyu-register-client` + `shenyu-register-client-api` + `shenyu-register-client-http` (16 files) | remove | Aggregator removed with both artifacts |
| Business starters | `shenyu-spring-boot-starter/shenyu-spring-boot-starter-client/{apache-dubbo,common,grpc,mcp,sofa,spring-websocket,springmvc,tars}` + aggregator (57 files incl. the two retained beat modules) | remove except beat | Eight starters leave; aggregator must survive while beat is its child, then be retired after the reparent below |
| Beat registration | `shenyu-register-center/shenyu-register-client-beat` | retain | Gateway runtime; excluded by the guard's `client_artifact()` |
| Beat starter | `shenyu-spring-boot-starter/shenyu-spring-boot-starter-client/shenyu-spring-boot-starter-client-beat` | retain, re-parent | Parent moves from `shenyu-spring-boot-starter-client` to `shenyu-spring-boot-starter`; artifact coordinates and heartbeat behavior unchanged |
| Staged examples | 19 inventoried roots under `shenyu-examples/` | remove | `shenyu-examples-plugin-contract` (added after the baseline) is main-owned contract testing and stays |
| Staged dependency management | `staged_dependency_management` block in `shenyu-examples/pom.xml` | remove | Drops with the example removal step |

Tests are removed with their module; no test moves to main because the external client repository owns them after the handoff. Nothing is deleted here: every removal row executes only in the gated source-removal PR.

## Retained consumers (must keep working)

- `shenyu-bootstrap/pom.xml` depends on `shenyu-spring-boot-starter-client-beat` — preserved by the beat retention and reparent.
- `ci.yml` runs `check-client-boundary.py` twice (once with `--trees`) and builds `shenyu-spring-boot-starter-client-beat -am` for the dependency-tree check; both stay valid after the reparent, and the `modules: shenyu-client,shenyu-register-center,shenyu-registry` build lists drop only `shenyu-client` while keeping `shenyu-register-center` (the beat module lives there).
- `script/verify_spring_boot_auto_configuration.py` skips `/shenyu-spring-boot-starter-client/` paths; after removal the skip narrows to the retained beat starter so the verifier keeps covering it.
- `check-client-boundary.py` already hard-excludes both beat artifacts from `client_artifact()`; the removal PR only empties the inventory exemption lists, keeping the guard in place as the permanent "no business-client references in main" check.

## Removal order (each step keeps the guard and the reactor green)

1. Remove the 19 example roots and their reactor entries plus the `staged_dependency_management` block, coordinating so no surviving example references an already-removed module.
2. Remove the eight business starters and `shenyu-client` / `shenyu-register-client-api` / `shenyu-register-client-http` with their module entries in the root, `shenyu-register-center` and `shenyu-spring-boot-starter` poms; drop `shenyu-client` from the CI `modules:` lists in the same step.
3. Re-parent `shenyu-spring-boot-starter-client-beat` to `shenyu-spring-boot-starter` and retire the now-empty `shenyu-spring-boot-starter-client` aggregator; narrow the auto-configuration verifier skip.
4. Empty the inventory exemption lists, remove the obsolete workflows/scripts/Docker/Compose/K8s paths and docs references, and let `check-client-boundary.py` reject any new business-client reference, including inactive profiles.

## Gates before execution

- [ ] #8421 single-owner artifact handoff published (currently open)
- [ ] #8422 runtime verification for WebSocket, Dubbo, SOFA and TARS registration (currently open; an open PR covers part of it)
- [ ] #8420 CI/gateway-test decoupling from client-owned examples (currently assigned and in progress)
- [ ] #6366 compatibility evidence for the declared release scope (currently open, no evidence recorded)

The gate status above was re-checked on 2026-11-26; this manifest is updated whenever the accepted client revision or the inventory changes, and the removal PR will link each gate's evidence in its description.
