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

package org.apache.shenyu.admin.service.openapi;

import org.apache.shenyu.admin.model.dto.AgentGatewayOpenApiRequest;
import org.apache.shenyu.admin.model.dto.AgentGatewayOpenApiRequest.Apply;
import org.apache.shenyu.admin.model.result.AgentGatewayOpenApiPreview;

/**
 * Agent Gateway OpenAPI import: preview and explicit publication through the existing rule flow.
 */
public interface AgentGatewayOpenApiService {

    /**
     * Read-only preview, including unsupported selections and possible source deletions.
     *
     * @param request explicit document and mappings
     * @return independent preview
     */
    AgentGatewayOpenApiPreview preview(AgentGatewayOpenApiRequest request);

    /**
     * Recheck the complete preview after locking the target rule, then publish selected changes.
     *
     * @param apply exact preview request, revision and selected source identities
     * @return new preview after successful publication
     */
    AgentGatewayOpenApiPreview apply(Apply apply);
}
