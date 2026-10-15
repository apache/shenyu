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

# Cache request eligibility

The cache plugin caches responses only for GET requests with an empty request
body. POST, PUT, PATCH, DELETE, HEAD, OPTIONS, and other methods continue through
the plugin chain without reading or writing response-cache entries. Requests are
forwarded normally; they are not rejected because they cannot be cached.

A GET request with a positive Content-Length or Transfer-Encoding also bypasses
the cache. For other GET requests, the plugin checks the actual body stream.
If data arrives, the complete stream, including its first buffer, is forwarded
without aggregating it. A request-body error is propagated instead of being
hidden by a cached response.

Cache keys now use a versioned combination of the HTTP method and the complete
raw URI, including scheme, authority, path, and raw query. Different authorities
and encoded query parameters therefore do not share entries. Existing keys from
the previous path/query-only format are not reused and expire according to their
configured TTL. No Redis key migration is required.

Routes that previously cached POST or other non-GET requests now call their
upstream on every request. Body-sensitive response caching requires an explicit
cache-key design before it can be enabled again.
