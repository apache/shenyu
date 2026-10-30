# Agent Gateway resource admission

Agent Gateway applies deterministic admission around every `tools/call` execution. Admission is deliberately not a retry or circuit-breaking layer: a call still executes at most once.

## Configuration

The trusted plugin configuration may contain one `admission` object:

```json
{
  "admission": {
    "globalInFlight": 128,
    "globalQueued": 64,
    "localInFlight": 32,
    "localQueued": 16,
    "remoteInFlight": 8,
    "remoteQueued": 16
  }
}
```

Omitting the object or supplying an empty object selects these defaults. Admin and gateway reject unknown fields and values outside the hard maxima (`128`, `256`, `64`, `128`, `32`, and `128` respectively).

## Semantics

- `global*` bounds all concurrent local and remote tool executions and their waiting work.
- `local*` bounds startup and request-local HTTP tool providers together.
- `remote*` is applied independently for each configured remote MCP server name.
- Admission checks the absolute request deadline before granting each global/local/remote permit, before reporting queue overflow, and immediately before subscribing to execution. An already-expired request never executes its provider, even with free capacity. Time spent waiting counts against execution time.
- Queue overflow returns HTTP `429` and JSON-RPC `-32026`; an expired request returns HTTP `504` and `-32026`.
- Every success, business error, gateway error, cancellation, timeout, configuration withdrawal and shutdown releases each acquired permit once.
- Local validation runs before resource admission. The execution-attempt callback and provider invocation run only when the admitted action is subscribed. Overload, queue timeout and cancellation before admission produce no execution-attempt audit record; their logical terminal record remains available.
- Existing byte limits, transport concurrency, catalog generation leases and no-retry behavior remain in force.

Configuration reload preserves active and queued accounting. All scope limits are installed before newly available slots are drained. Reload draining, terminal release and shutdown follow the same controller-promotion-monitor then scope-monitor order, including synchronous completion of promoted actions. Remote scope collections are copied under their registry monitor before callbacks or snapshot reads acquire scope monitors.

## Metrics

The Prometheus exporter publishes payload-free aggregates by fixed scope (`global`, `local`, `remote`): `shenyu_agent_admission_inflight`, `shenyu_agent_admission_queued`, `shenyu_agent_admission_inflight_limit`, `shenyu_agent_admission_queue_limit` and `shenyu_agent_admission_rejections_total`. Tool names, subjects, tenants and endpoints are never labels.

## Sizing guidance

Keep the global budget at or below the gateway process's sustainable CPU and response-memory budget. Reserve a separate remote in-flight budget for each latency-critical MCP server, and keep remote queues small: queued remote calls consume a shared global slot while waiting. Increase queue sizes only when clients can tolerate waiting until the existing request deadline; admission does not extend deadlines.
