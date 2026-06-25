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

import jakarta.validation.Valid;
import org.apache.shenyu.admin.aspect.annotation.RestApi;
import org.apache.shenyu.admin.model.dto.UpstreamManualStatusDTO;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.service.DiscoveryUpstreamService;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.enums.UpstreamManualStatusEnum;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;

/**
 * Upstream controller.
 */
@RestApi("/upstream")
public class UpstreamController {

    private final DiscoveryUpstreamService discoveryUpstreamService;

    public UpstreamController(final DiscoveryUpstreamService discoveryUpstreamService) {
        this.discoveryUpstreamService = discoveryUpstreamService;
    }

    /**
     * manual offline.
     *
     * @param upstreamManualStatusDTO upstream request
     * @return result
     */
    @PostMapping("/offline")
    public ShenyuAdminResult offline(@Valid @RequestBody final UpstreamManualStatusDTO upstreamManualStatusDTO) {
        discoveryUpstreamService.changeManualStatusBySelectorIdAndUrl(
                upstreamManualStatusDTO.getSelectorId(),
                upstreamManualStatusDTO.getUrl(),
                UpstreamManualStatusEnum.FORCE_OFFLINE);
        return ShenyuAdminResult.success(ShenyuResultMessage.UPDATE_SUCCESS);
    }

    /**
     * manual online.
     *
     * @param upstreamManualStatusDTO upstream request
     * @return result
     */
    @PostMapping("/online")
    public ShenyuAdminResult online(@Valid @RequestBody final UpstreamManualStatusDTO upstreamManualStatusDTO) {
        discoveryUpstreamService.changeManualStatusBySelectorIdAndUrl(
                upstreamManualStatusDTO.getSelectorId(),
                upstreamManualStatusDTO.getUrl(),
                UpstreamManualStatusEnum.NONE);
        return ShenyuAdminResult.success(ShenyuResultMessage.UPDATE_SUCCESS);
    }
}
