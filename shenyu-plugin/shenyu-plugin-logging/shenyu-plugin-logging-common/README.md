<!--
Licensed to the Apache Software Foundation (ASF) under one or more
contributor license agreements. See the NOTICE file distributed with
this work for additional information regarding copyright ownership.
The ASF licenses this file to You under the Apache License, Version 2.0
(the "License"); you may not use this file except in compliance with
the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-->

# Logging queue metrics

When ShenYu's metrics starter is enabled (`shenyu.metrics.enabled=true`),
the existing Prometheus endpoint exposes these metrics for running collectors
that extend `AbstractLogCollector` (Kafka, Elasticsearch and RocketMQ):

| Metric | Type | Meaning |
| --- | --- | --- |
| `shenyu_logging_queue_accepted_total` | Counter | Records successfully admitted to a logging queue. This is not a backend delivery count. |
| `shenyu_logging_queue_dropped_total` | Counter | Records rejected at queue admission because the queue has reached its capacity. |
| `shenyu_logging_queue_size` | Gauge | Records currently in the queues, excluding batches already drained for sending. |
| `shenyu_logging_queue_capacity` | Gauge | Effective admission capacity in records, summed across the queues in the indicated mode. |

The only labels are `collector` (the fully qualified collector implementation
class) and `mode` (`single` or `multi`). Selector IDs, paths, request IDs and
backend addresses are never labels. In multi-client mode, measurements aggregate
all selector queues for the collector. Consequently, an individual selector
queue can overflow even when the aggregate occupancy is below aggregate capacity;
use the dropped counter to alert on any admission loss.

For single-client mode, capacity is the live configured `bufferQueueSize`, not
the physical capacity of the underlying unbounded deque. After shrinking the
configuration, occupancy may temporarily exceed capacity while the existing
backlog drains. For multi-client mode, capacity is the sum of the actual bounds
chosen when each selector queue was created. Existing queues retain their bounds
after configuration changes; new queues use the latest configuration. Before the
first selector queue is created, aggregate capacity is zero. Inactive modes have
zero admission capacity but still expose any backlog retained after a mode switch.

Only attempted admissions are counted. Rule sampling, null logs, collectors that
have not started or have stopped, and missing clients do not increase the dropped
counter. Backend send failures are also excluded: a record already accepted can
still fail during delivery.

Counters are retained across stop/start cycles of the same collector instance.
Stopped collectors are removed from the exported series, and a new collector
instance or gateway process starts with new counters. Metrics work regardless of
whether the logging collector or the metrics service starts first. Gauges read
live queue state at scrape time, so idle collectors do not retain stale occupancy
values after draining. Concurrent scrapes are observations rather than atomic
snapshots across all queues and counters.

Admission uses the existing non-waiting capacity checks and `offer` behavior.
Instrumentation adds only thread-safe counters to the request path; it adds no
worker, auxiliary record queue or backend metrics dependency to logging plugins.
