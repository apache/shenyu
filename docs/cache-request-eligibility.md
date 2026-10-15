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

# Cache eligibility and response replay

The cache plugin caches responses only for GET requests with an empty request
body. POST, PUT, PATCH, DELETE, HEAD, OPTIONS, and other methods continue through
the plugin chain without reading or writing response-cache entries. Requests are
forwarded normally; they are not rejected because they cannot be cached.

Requests with Authorization, Cookie, Range, conditional headers (If-Match,
If-None-Match, If-Modified-Since, If-Unmodified-Since, If-Range), Cache-Control,
or Pragma also bypass cache reads and writes. The upstream handles these request
conditions; the plugin does not implement range selection or revalidation.

A GET request with a positive Content-Length or Transfer-Encoding also bypasses
the cache. For other GET requests, the plugin checks the actual body stream.
If data arrives, the complete stream, including its first buffer, is forwarded
without aggregating it. A request-body error is propagated instead of being
hidden by a cached response.

Cache keys use a versioned combination of the HTTP method, complete raw URI,
matched selector ID, and matched rule ID. Different authorities, encoded query
parameters, and routing rules therefore do not share entries.

Each response is stored as one value containing its status, end-to-end headers,
and raw body bytes. This preserves redirects, error statuses, multi-value headers,
and responses without a Content-Type. Content-Length is recalculated; connection
headers and headers named by Connection are not replayed. Empty responses,
including 204, are supported.

The snapshot is taken at response commit, after preceding beforeCommit callbacks,
and stored only after the response write succeeds. Responses with Set-Cookie or
response cookies, Vary, private, no-store, or no-cache directives are not stored.
Informational, partial (206), and not-modified (304) responses are also excluded.
Streaming responses written with writeAndFlushWith are forwarded without
buffering them into cache entries.

The complete-response format uses a separate versioned key. Legacy body-only or
body/Content-Type entries are not reused and expire according to their configured
TTL. Invalid entries are treated as misses. No Redis key migration is required.

Routes that previously cached POST or other non-GET requests now call their
upstream on every request. Body-sensitive response caching requires an explicit
cache-key design before it can be enabled again.
