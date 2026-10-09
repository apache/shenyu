# Apache ShenYu architecture diagrams

These diagrams complement the high-level architecture image used in the
project README. They provide focused views of the control plane, configuration
synchronization, request execution, and plugin delivery model.

| Diagram | Description |
| --- | --- |
| [Overall architecture](shenyu-overall-architecture.svg) | Entry points, control plane, gateway runtime, plugin capabilities, and delivery paths. |
| [Control plane](shenyu-control-plane-architecture.svg) | Admin configuration management, event dispatch, and synchronization channels. |
| [Configuration synchronization](shenyu-config-sync-flow.svg) | Data changes from Admin through synchronization channels into gateway caches. |
| [Request runtime](shenyu-request-runtime-flow.svg) | Request ingress, context construction, selector/rule matching, plugin execution, and response streaming. |
| [Plugin and delivery map](shenyu-plugin-delivery-map.svg) | Plugin families and the starter, distribution, example, and test modules that deliver them. |

The labels inside the focused diagrams are in Chinese. Keep each diagram in
sync with the corresponding runtime or control-plane implementation when those
flows change.
