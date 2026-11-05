/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.shenyu.admin.model.result;

import java.util.List;
import java.util.Map;

/** Preview is data only. Tool plans are JSON strings to avoid exposing Gson internals through Jackson. */
public record AgentGatewayOpenApiPreview(String revision, List<Item> items, List<String> warnings) {

    /** One independently selectable change or diagnostic. Before and after include request mappings. */
    public record Item(String identity, String toolName, String action, String before, String after, List<String> diagnostics) {
    }

    /** Pure generator result, including all source identities so deselection does not imply deletion. */
    public record Generated(Map<String, Item> items, List<String> sourceIdentities) {
    }
}
