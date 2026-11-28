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

package org.apache.shenyu.admin.controller;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.aspect.annotation.RestApi;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.service.MatchingConditionMetadataService;
import org.apache.shenyu.common.condition.MatchingConditionCatalog;
import org.apache.shenyu.common.exception.CommonErrorCode;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;

/**
 * Read-only matching-condition metadata controller.
 */
@RestApi("/matching-conditions")
public class MatchingConditionController {

    private static final String UNSUPPORTED_VERSION_MESSAGE = "Unsupported matching condition catalog version";

    private final MatchingConditionMetadataService matchingConditionMetadataService;

    /**
     * Creates the matching-condition metadata controller.
     *
     * @param matchingConditionMetadataService metadata service
     */
    public MatchingConditionController(final MatchingConditionMetadataService matchingConditionMetadataService) {
        this.matchingConditionMetadataService = matchingConditionMetadataService;
    }

    /**
     * Returns the current matching-condition catalog.
     *
     * @param version optional requested catalog version
     * @return Admin result containing the catalog or a bounded unsupported-version error
     */
    @GetMapping
    public ShenyuAdminResult catalog(@RequestParam(name = "version", required = false) final String version) {
        if (StringUtils.isNotBlank(version) && !MatchingConditionCatalog.VERSION.equals(version)) {
            return ShenyuAdminResult.error(CommonErrorCode.NOT_FOUND_EXCEPTION, UNSUPPORTED_VERSION_MESSAGE);
        }
        return ShenyuAdminResult.success(matchingConditionMetadataService.catalog());
    }
}
