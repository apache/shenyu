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

# Gateway API Namespace selectors

Source: [SPEC-8403001](https://github.com/apache/shenyu/issues/8403#issuecomment-6760387073)

## Requirement: Namespace selector route attachment

The Gateway API controller MUST authorize from=Selector against the route Namespace's current labels with Kubernetes matchLabels and matchExpressions semantics (In, NotIn, Exists, DoesNotExist), combining requirements with AND. An empty selector MUST match a present Namespace; missing Namespace or malformed/missing selector MUST fail closed. Namespace add, label change and delete MUST trigger affected routes to grant, revoke or restore runtime config and listener bindings without editing routes or waiting for resync. Parent policy denials MUST report Accepted=False/NotAllowedByListeners, and affected Gateway listener attachedRoutes MUST be refreshed after binding cache updates. Same/All, other listener gates, independently accepted parents, backend ReferenceGrant authorization and legacy Ingress MUST retain existing semantics.

### Scenario: matching namespace

- **WHEN** a route Namespace satisfies all selector requirements and other listener gates
- **THEN** the route attaches and is programmed, including match-all empty selectors

### Scenario: revocation and recovery

- **WHEN** Namespace labels change, Namespace is deleted, or missing Namespace is created
- **THEN** affected routes reconcile against current cache, remove or restore config and bindings, report parent status and queue Gateway counts after cache mutation

### Scenario: fail closed

- **WHEN** Namespace data is missing or selector syntax, operators, keys, values or cardinalities are invalid
- **THEN** that listener cannot authorize attachment or broaden access

### Scenario: independent acceptance

- **WHEN** one listener or parent rejects but another accepts
- **THEN** remaining authorized config and bindings survive with appropriate per-parent status

### Scenario: preserved behavior

- **WHEN** Same, All, backend ReferenceGrant or legacy Ingress behavior is exercised
- **THEN** existing semantics remain and Namespace data is not required for Same or All
