# AI Concurrency Admission (issue #8254)

Opt-in, per-gateway concurrency admission for AI proxy traffic: trusted-tenant
and model-pool in-flight caps with finite bounded waiting, all inside the shared
request deadline. This is the first delivery of #8254 — fixed caps with
deterministic FIFO queue discipline; adaptive control (#7930), distributed
quotas (#7957) and token/cost budgets (#7702/#7958) remain with their own tasks.

## Enabling

`shenyu.plugins.ai.proxy.admission.enabled=true` (default `false` — with the
feature off the gateway holds no admission state and behaves byte-identically to
today). Bounds: `tenantMaxInflight` / `tenantMaxWaiting` / `modelMaxInflight` /
`modelMaxWaiting` / `maxWaitMillis` / `maxPools`; a misconfigured bound fails
fast at startup. Per-selector tuning is a follow-up on the `AiProxyHandle`
extension point.

## The two-pool permit model

| Permit | Pool | Key | Held for | Released |
|---|---|---|---|---|
| logical | tenant pool | trusted tenant from the authenticated `AgentRequestContext`; anonymous traffic shares one pool | the whole invocation — retries, fallbacks and repairs | once, at the terminal signal |
| target | model pool | effective model after overrides (carried on `PreparedExchange`) | one actual attempt — for streaming, until body completion, error or cancellation | at the end of each attempt |

A failed attempt releases its own target capacity; backoff between attempts
holds no target permit; a fallback attempt keys on the fallback model. The
identity never comes from wire metadata — a null tenant is anonymous and shares
one pool, never an IP or header.

## Waiting and the shared deadline

Every pool is a fair semaphore with a finite waiting bound; new arrivals beyond
the bound are rejected immediately. The effective wait is the smaller of
`maxWaitMillis` and the shared-deadline remainder, so time already queued counts
against `requestDeadline` (#8121): an expired or cancelled waiter starts no work,
and the 3 s/2.8 s scenario cannot grant a fresh 5 s allowance. Waiting happens
off the event loop on the bounded-elastic scheduler, bounded by the waiting
limits. All permit decisions run at subscription time, never at assembly, so a
retry attempt observes its predecessor's release.

## Rejection → client outcome

| Outcome | Attempt event reason | Client status |
|---|---|---|
| waiting bound reached / pool map cap / expired deadline | `ADMISSION_REJECTED` | 429 TOO_MANY_REQUESTS |
| bounded wait expired | `ADMISSION_WAIT_DEADLINE_EXCEEDED` | 429 TOO_MANY_REQUESTS |

Rejections happen before any upstream call and map to the terminal observation
as `RATE_LIMITED`. The pool map is hard-capped (`maxPools`); in-flight and
waiting gauges are aggregated per pool in-process and are never exported as raw
tenant or model metric labels.

## Layering and boundaries

The Netty transport pool (`AiProxyTransportProperties.pool`) bounds TCP
connections process-wide and stays untouched — admission is request-level and
identity-aware, one layer above it. Configuration is per-process static: a
reload replaces the component, and in-flight executions release against the
instance they acquired from, so outstanding capacity is never reset or stranded.
Per-gateway by design: replicas expose their own queues and an operator
aggregates across them; this is not a distributed hard limit.

## Follow-ups

Per-selector admission tuning, #8046 logical-model references as pool keys,
metric endpoint exposure of the pool gauges, and the #7930 latency-adaptive
control once its contract lands.
