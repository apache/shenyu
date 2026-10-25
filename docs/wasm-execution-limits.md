<!--
Licensed to the Apache Software Foundation (ASF) under one or more
contributor license agreements.  See the NOTICE file distributed with
this work for additional information regarding copyright ownership.
The ASF licenses this file to You under the Apache License, Version 2.0
(the "License"); you may not use this file except in compliance with
the License.  You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-->

# WASM execution deadlines and linear-memory budgets

WASM limits are opt-in startup settings for #7560. The default remains zero
(disabled) for deadlines and memory ceilings, preserving trusted deployments.

Set JVM properties through the gateway/Admin launcher's JVM options:

```text
-Dshenyu.wasm.execution-timeout-millis=1000
-Dshenyu.wasm.initialization-timeout-millis=10000
-Dshenyu.wasm.max-memory-pages=256
```

A page is 64 KiB: 256 pages permit 16 MiB of linear memory per pooled instance.
A loader has at most eight instances by default, so account for its pool size
and the number of plugin/handler loaders when sizing the process. This ceiling
does not bound Java heap, tables, WasmGC objects, compiler output, payload buffers
or arbitrary allocations inside host callbacks.

The same settings can be overridden for a resource-owning class with
shenyu.wasm.<fully-qualified-class-name>.<setting>. For example, append
.execution-timeout-millis to that prefix. Properties are read once when each
WasmLoader is constructed; reload/restart the owning component to change them.
Validate nonnegative deadlines and memory pages in 0–65536. Invalid configuration
fails construction. With a positive export deadline, initialization defaults to
10 seconds unless explicitly set. A zero initialization deadline deliberately
disables the construction deadline even if exports have a deadline.

## Execution and cancellation

When deadlines are enabled, guest work runs on an owned daemon executor, bounded
to the loader's instance-pool size, with at most one pool-size worth of pending
calls. Saturation fails; caller-runs execution and automatic replacement threads
are not used. Export deadlines include waiting for executor/instance capacity.
Compilation, instance creation/data initialization and _initialize calls each
have the separately configured construction deadline.

Both the Chicory compiler and interpreter execute under the same limiter.
The runtime's interruption behavior is documented in
[Chicory CPU limits](https://chicory.dev/docs/usage/cpu/); tests use actual
infinite-loop guests in both modes, rather than timing out a synthetic publisher.

A timeout cancels the actual worker future with interruption and returns
ShenyuWasmException with a timeout message. Interruption of a waiting caller
also cancels its guest and preserves the caller's interrupt status. Closing a
loader interrupts its tracked calls, shuts down its executor and rejects new calls.
Errors and capacity rejection propagate through existing WASM/gateway error handling;
this change does not introduce a dedicated HTTP status or retry policy.

An interrupted, cancelled or trapped instance is not reused: its pool slot remains
occupied until execution physically exits, and the next call constructs/initializes
a fresh instance before executing. Tests dirty the timed-out guest's memory and
verify the next invocation sees a clean instance and returns successfully.
Nested host-to-guest calls stay within the outer worker/deadline and existing
instance-pool nesting rules.

Request execute/doExecute publishers are deferred and offloaded when export
deadlines are enabled, so subscription cancellation reaches the waiting caller
and then the guest future. Compatibility before/after hooks and data-handler
callbacks remain synchronous APIs with bounded waits; they do not run guest code
on the invoking thread. Default-disabled request construction semantics are retained.

A custom host callback that ignores interruption cannot safely be forcibly stopped
inside the JVM. It retains its worker and instance capacity until physical exit;
further work stays bounded and fails/times out instead of creating more threads.
If the host never exits, that worker is permanently occupied. The configured
pool size caps the number of such calls; timed-out workers are never replaced.
Use trusted cooperative host functions, or process isolation when hard termination
of arbitrary host/native work is required. Existing argument maps still remove
their entries in finally after each invocation result/error; custom adapters must
not continue using request arguments after cancellation.

## Memory policy

The configured ceiling is the minimum of the module-declared maximum and the
gateway budget. It never raises a module's declared maximum or lowers its initial
allocation silently. An initial allocation above the budget fails initialization.
memory.grow beyond the ceiling follows WASM semantics and returns -1; later valid
requests still work.

A budget permits zero or one module-owned, non-shared linear memory. A memory-free
module can execute exports, but getMemory() fails explicitly inside its callbacks.
This also changes the failure point for memory-free guests without a configured
budget: loading now succeeds, and only a callback that accesses memory fails;
previously the guest was rejected during initialization.
Imported, shared or multiple memories are rejected with a configuration error
when budgeting is enabled, because those layouts require a separate aggregate/
ownership contract. Disabled budgeting retains existing runtime behavior.

## Verification and rollback

The WAT fixtures under shenyu-plugin-wasm-api/src/test/wasm-limits are rebuilt by
the pinned WABT 1.0.42 fixture workflow. Tests cover actual infinite loops in both
execution modes, memory.grow, rejection of excessive initial memory, caller
cancellation reaching a host callback, interruption-ignoring host containment,
initialization timeout and successful subsequent calls.

Run the Maven wrapper with -pl shenyu-plugin/shenyu-plugin-wasm-base -am test
and select *Wasm*Test. Keep fixture-rebuild and RAT checks in the change.
To roll back the feature, remove the JVM properties or set them to zero and
recreate the loaders/restart. Do not raise or disable budgets automatically after
a timeout or trap.
