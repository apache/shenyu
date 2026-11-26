# Coordinate ownership handoff manifest

Planning slice of [#8421](https://github.com/apache/shenyu/issues/8421): the single-owner Maven artifact handoff for the Java client migration tracked by [#7602](https://github.com/apache/shenyu/issues/7602). The machine-readable source of truth is [`.github/coordinate-ownership.json`](../.github/coordinate-ownership.json); this document explains how to read it, how the release lane enforces it, and how to activate or roll back the handoff. The per-module removal order lives in the separate [disposition manifest](client-migration-disposition.md) for #8423; the two documents share the same coordinate set.

## Ownership

Every Maven coordinate under `org.apache.shenyu` is main-owned by default. The manifest records the exception: the transferred set moving to `apache/shenyu-client-java` — the `shenyu-client` family (15 leaf artifacts across the nested aggregators), `shenyu-register-client-api` / `shenyu-register-client-http` with their aggregator, and the eight business starters with their parent — plus the foundations main keeps publishing so the independent client can resolve them: `shenyu-register-protocol`, `shenyu-register-common`, `shenyu-register-mcp`, and the two heartbeat artifacts whose names say client but whose ownership is gateway runtime. The `shenyu-sdk` family stays main-owned via the prefix rule.

The selected foundation version for consumers of the accepted client revision is **2.7.2** (the latest released main version; main itself is 2.7.3-SNAPSHOT). The accepted client revision is the merge of [apache/shenyu-client-java#16](https://github.com/apache/shenyu-client-java/pull/16); until that merge lands, the manifest records only the candidate baseline and no publication boundary.

## Release-lane enforcement (non-publishing)

The release candidate build in `ci.yml` runs `check-coordinate-ownership.py` against the freshly built local Maven repository *before* anything is signed or staged to Nexus:

- In the current `preparation` state the check verifies that the manifest matches the reactor (every staged inventory artifact is declared transferred, every declared coordinate exists) and that the retained foundations were actually built. Main keeps publishing the staged coordinates, so consumers are never stranded before replacement artifacts exist.
- Boundary records must stay empty while the state is `preparation`, and the release version must be neither a SNAPSHOT nor carry a preparation marker — a candidate cannot pass as release-ready.
- After the state flips to `transferred`, the same check rejects any transferred coordinate found in the built repository: main can no longer publish them, and the boundary records must list the final main-published version of every transferred coordinate so the first client-owned version has a documented starting point.

The dry-run never deploys anything. Activating the ownership switch (flipping `handoff_state`) is a release-manager action that follows the Apache release process, taken only after #6366's compatibility evidence and the published replacement artifacts exist.

## Activation checklist

1. Confirm [apache/shenyu-client-java#16](https://github.com/apache/shenyu-client-java/pull/16) is merged and resolves the selected foundation version 2.7.2 from the staged Apache repositories.
2. Publish the independent client artifacts under `org.apache.shenyu` coordinates from `apache/shenyu-client-java`.
3. Record `last_main_version` for every transferred coordinate (the final main release that still published it).
4. Flip `handoff_state` to `transferred`; the next release-candidate build then fails if any transferred coordinate is still produced by main.
5. Execute the [removal manifest](client-migration-disposition.md) for #8423 once the remaining gates close.

## Rollback

Before publication, reverting `handoff_state` to `preparation` and dropping the boundary records restores the previous release content exactly; nothing was published. After publication, consumers are pointed back to the recorded `last_main_version` of each coordinate; that version is never republished and ownership is not silently transferred back — an abort is a project vote that cuts a *new* main-published version, keeping Admin wire compatibility through the unchanged `shenyu-register-protocol` contract.

## Guard coverage

`check-coordinate-ownership.py` (with the regression fixtures in `test-coordinate-ownership.py`) runs on every pull request, so drift between the inventory, the reactor and this manifest fails CI before it can reach a release candidate. The four negative fixtures pin the issue's required scenarios: preparation state must not pass as release-ready, a missing foundation fails, a transferred coordinate in the wrong publisher fails, and a valid ownership selection passes.
