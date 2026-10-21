# ShenYu Admin API Document

You can check http://localhost:9095/swagger-ui.html to check all restful api and models.

## Client discovery registration

URI registration creates a selector-level local discovery as a fallback. If a
client subsequently registers Zookeeper, Nacos or another supported remote
discovery, Admin replaces only this automatically created fallback and clears its
local upstream observations. A committed takeover also clears the previous runtime
upstreams when the remote registry is empty. The existing handler/relation identity is preserved;
the configured discovery supplies the new upstreams after the transaction commits.

Once a remote discovery owns a selector, later URI registrations do not inject
local upstreams or overwrite the selector handle. Existing manually configured
discoveries are preserved. Registration retries use the same transaction boundary,
and the URI registration's discovery event is published only after commit.

The selector lock coordinates concurrent Admin writers without blocking unrelated
selectors during URI health checks. This prevents new duplicate initial bindings;
pre-existing duplicate relations are not automatically merged.
