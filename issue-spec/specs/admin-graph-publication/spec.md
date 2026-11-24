# Admin graph publication

The Admin graph API manages a graph within one namespace. A draft is the
editable source. A published revision records the desired configuration for
gateways that select that graph. The namespace and graph ID in the request
path identify the resource on every operation.

## Editing and validation

Use `/graph/{namespaceId}` as the base path. Graph editing and publication
require graph edit permission. Reading drafts, history, validation results
and published snapshots requires graph view or edit permission. A delegated
dashboard user must also belong to the namespace. When the dashboard password
policy protects `system:graph:edit`, it checks the super administrator's password
before creating or replacing a draft, publishing, rolling back or withdrawing a graph.

| Method and path | Operation |
| --- | --- |
| `POST /{graphId}/draft` | Create a draft from a graph definition. |
| `GET /{graphId}/draft` | Read the saved draft and its version. |
| `PUT /{graphId}/draft` | Replace the saved definition at the expected version. |
| `POST /{graphId}/validate` | Check the saved draft against the namespace's plugin configuration. |
| `POST /{graphId}/publish` | Validate and publish the saved draft. |
| `GET /{graphId}/revisions` | Read immutable publication history. |
| `GET /{graphId}/revisions/{revision}` | Read one publication or withdrawal. |
| `POST /{graphId}/revisions/{revision}/rollback` | Revalidate and publish the selected historical content under a new revision. |
| `DELETE /{graphId}` | Withdraw the graph and clear its draft content. |
| `GET /snapshot` | Read the current publication for each published graph in the namespace. |

Create and replace accept the shared graph definition as JSON. The namespace
and graph identity in that definition must match the path. A named
`deploymentBinding` limits a graph to the named deployment in that namespace.
An omitted binding applies to the namespace. Admin assigns published revision
numbers and content hashes.

Replace, publish, rollback and delete take the `expectedDraftVersion` query
parameter. Each successful change advances the draft version. Two callers
starting at the same version can commit at most one change. After a conflict,
read the draft again before deciding whether to retry. Editing a draft leaves
the current publication available while the changes are being prepared.

Draft and revision responses carry the definition as a nested `graph` object.
The draft starts at `draftVersion: 1` and `publishedRevision: 0`. Validation
returns a list of violations, with an empty list meaning that the saved draft
passed Admin's checks. Instance-specific plugin and condition extensions are
resolved when the gateway binds the graph. Admin includes the standard gateway
starter plugins for context, URI resolution, HTTP transport, RPC parameter
transformation and response writing when they have no namespace configuration.
Explicit namespace disablement takes precedence. The gateway resolves the
transport selected by its own HTTP client configuration.

## Publication and recovery

A successful publication appends the complete graph definition to immutable
history and advances the current revision in one database transaction. An
invalid draft or a failed transaction leaves the previously published revision
available. Rollback uses current validation rules and plugin configuration.
For example, restoring revision 2 after revision 5 creates revision 6 with
the selected historical content. The previous revisions remain readable, and
a rollback preserves any separately edited live draft.

Deletion publishes a higher revision with a withdrawal marker. A consumer
reading a full namespace snapshot after an Admin restart can reconstruct both
the desired graphs and withdrawals from the committed rows. A consumer must
also remove a previously selected graph when its new binding no longer matches
the deployment. A failed or partial snapshot read must be retried before
replacing local state. Revision numbers order updates per graph, including
when a rollback has the same content hash as a previous publication.

After withdrawal the head retains its write token and the revision has a null
graph. An editor can submit a replacement draft using that token or restore a
previously published graph. Deleting the owning namespace removes its graph
data and membership grants. A namespace created later with the same business
identifier requires fresh grants and starts with its own history.

Publication only reports what Admin committed. An instance separately receives
the revision, checks its installed plugins, compiles it and reports activation.
Publishing, withdrawing and rolling back leave executor selection and legacy
plugin order untouched. Switching a deployment back to LEGACY uses deployment
configuration. Graph distribution and instance activation are tracked by
[#7998](https://github.com/apache/shenyu/issues/7998) and
[#7999](https://github.com/apache/shenyu/issues/7999).
