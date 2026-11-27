# gRPC bridge response aggregation

The HTTP/JSON bridge returns one aggregate after the upstream RPC completes.
This applies to responses from unary, server-streaming, client-streaming and
bidirectional RPCs.

## Configure the budget

The `grpc` plugin configuration accepts `maxAggregatedResultCount` and
`maxAggregatedResultBytes`. Each is unlimited when absent, null, zero or
negative. An RPC reads both limits from the same configuration when it starts.
Configuration updates affect subsequent calls.

Each admitted JSON object or blank string counts as one item. The byte counter
measures the UTF-8 payload of trimmed JSON objects and blank strings as retained.
Exceeding either budget fails the RPC, discards the aggregate and cancels the
upstream call. The bridge sends no partial success response. Cancellation cannot
reverse work the upstream has already performed.

For bounded deployments, configure both limits and a finite RPC deadline.
Size the budgets using representative upstream responses and the expected number
of concurrent RPCs. The gRPC channel separately limits each inbound message to
100 MiB. The aggregator checks its budget after protobuf decoding and before
parsing the response into a JSON object. The decoded values and the HTTP response
encoder can require additional heap.

## Observe terminal calls

The Prometheus exporter exposes `shenyu_grpc_aggregate_calls_total`,
`shenyu_grpc_aggregate_items_total` and
`shenyu_grpc_aggregate_payload_bytes_total`. Each series has one `outcome` label
with the fixed values `success`, `overflow`, `deadline`, `cancelled` and
`error`. The item and byte counters count data admitted before termination and
exclude the rejected item. These counters describe the upstream RPC. If an HTTP
client disconnects while the gateway writes an already completed aggregate, the
RPC remains a success.

## Local allocation measurements

The following measurements exercise `MessageWriter` in a Java 17.0.19 JVM
on macOS arm64 with a fixed 512 MiB heap. For allocation, reuse a
`JsonMessage.buildJsonMessage` instance containing a JSON object with 65,536
ASCII characters in one string and whitespace around the object. Measure
thread-allocated bytes across 1,500 new writers and results after warmup. Compare
the writer at `55e6f0d4c` with the change that checks the UTF-8 size before
copying or parsing the payload.

| Configured byte limit | Outcome | Before, allocated bytes per call | After, allocated bytes per call |
| ---: | --- | ---: | ---: |
| 1,024 | Rejected | about 134,000 | about 3,100 |
| 131,072 | Accepted | about 458,000 | about 392,000 |

For retained heap, hold the results of 32 completed calls in a list and divide
the difference in used heap after forced garbage collection by 32. Repeat with
one text field of 65,536 ASCII characters and with an array of 5,957 small
objects plus an empty object. A 65,548-byte text payload retained about 50,000
heap bytes per call. A 59,584-byte nested payload retained about 1,547,000 heap
bytes per call. The result shape determines the decoded heap cost.

These are process-local JVM estimates with prebuilt protobuf messages. They
exclude transport buffers, protobuf decoding and HTTP encoding. See the
[gRPC hardening task](https://github.com/apache/shenyu/issues/8150) and the
[gateway performance tracking issue](https://github.com/apache/shenyu/issues/7650).
