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

package org.apache.shenyu.admin.model.dto;

import jakarta.validation.Valid;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;

import java.util.List;
import java.util.Set;

/** Explicit operation selection and gateway mapping; never a request to expose a whole document. */
public record AgentGatewayOpenApiRequest(@NotBlank String namespaceId, @NotBlank String ruleId,
                                        @NotBlank @Size(max = 128) String sourceId,
                                        @NotBlank @Size(max = 1048576) String document,
                                        @NotNull @Size(max = 16) List<@Valid Mapping> mappings) {

    /** One selected operation; gatewayRuleId identifies an existing routing rule in the namespace. */
    public record Mapping(@NotBlank String method, @NotBlank String path, @NotBlank String gatewayPath,
                          @NotBlank String gatewayRuleId, String toolName) {

        /**
         * Source identity independent of operationId and gateway path.
         * @return exact method and source path
         */
        public String identity() {
            return method + " " + path;
        }
    }

    /** Apply re-generates the preview; it never accepts arbitrary tool JSON from a client. */
    public record Apply(@NotNull @Valid AgentGatewayOpenApiRequest request, @NotBlank String revision,
                        @NotNull Set<String> selected, boolean confirmRemovals, boolean acknowledgeCatalogLimit) {
    }
}
