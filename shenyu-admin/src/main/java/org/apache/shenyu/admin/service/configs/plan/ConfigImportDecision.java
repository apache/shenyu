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

package org.apache.shenyu.admin.service.configs.plan;

import java.util.List;

/**
 * One planning decision for a source object.
 *
 * @param sourceId the source object id from the archive
 * @param sourceName the source object business name
 * @param action the preview action
 * @param targetId the target id; this may be an ephemeral planned id
 * @param targetRef the report-safe target reference
 * @param remappedRefs reference remapping descriptions
 * @param diagnostics bounded diagnostic codes
 */
public record ConfigImportDecision(
        String sourceId,
        String sourceName,
        ConfigImportAction action,
        String targetId,
        String targetRef,
        List<String> remappedRefs,
        List<String> diagnostics) {

    /**
     * Defensive copy of reference and diagnostic lists.
     *
     * @param sourceId the source object id
     * @param sourceName the source object name
     * @param action the preview action
     * @param targetId the target id
     * @param targetRef the report-safe target reference
     * @param remappedRefs reference remapping descriptions
     * @param diagnostics diagnostic codes
     */
    public ConfigImportDecision {
        remappedRefs = List.copyOf(remappedRefs);
        diagnostics = List.copyOf(diagnostics);
    }
}
