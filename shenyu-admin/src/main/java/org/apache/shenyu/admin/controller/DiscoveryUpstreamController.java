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

import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.admin.aspect.annotation.RestApi;
import org.apache.shenyu.admin.mapper.DiscoveryHandlerMapper;
import org.apache.shenyu.admin.model.dto.DiscoveryUpstreamDTO;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.service.DiscoveryUpstreamService;
import org.apache.shenyu.admin.validation.annotation.Existed;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;

import jakarta.validation.Valid;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotEmpty;
import java.util.List;

@RestApi("/discovery-upstream")
public class DiscoveryUpstreamController {

    private final DiscoveryUpstreamService discoveryUpstreamService;

    public DiscoveryUpstreamController(final DiscoveryUpstreamService discoveryUpstreamService) {

        this.discoveryUpstreamService = discoveryUpstreamService;
    }

    /**
     * create discovery upstream.
     *
     * @param discoveryUpstreamDTO discoveryUpstreamDTO
     * @return {@linkplain ShenyuAdminResult}
     */
    @PostMapping("")
    public ShenyuAdminResult createDiscoveryUpstream(@Valid @RequestBody final DiscoveryUpstreamDTO discoveryUpstreamDTO) {
        return ShenyuAdminResult.success(discoveryUpstreamService.createOrUpdate(discoveryUpstreamDTO));
    }


    /**
     * create discovery upstream list.
     *
     * @param discoveryUpstreamDTOList discoveryUpstreamDTO
     * @return {@linkplain ShenyuAdminResult}
     */
    @PostMapping("batch")
    public ShenyuAdminResult createDiscoveryUpstreamList(@Valid @RequestBody final List<DiscoveryUpstreamDTO> discoveryUpstreamDTOList) {
        if (CollectionUtils.isNotEmpty(discoveryUpstreamDTOList)) {
            for (DiscoveryUpstreamDTO discoveryUpstreamDTO : discoveryUpstreamDTOList) {
                discoveryUpstreamService.createOrUpdate(discoveryUpstreamDTO);
            }
        }
        return ShenyuAdminResult.success();
    }

    /**
     * update discovery upstream.
     *
     * @param discoveryHandlerId   discoveryHandlerId
     * @param discoveryUpstreamDTO discoveryUpstreamDTO
     * @return {@linkplain ShenyuAdminResult}
     */
    @PutMapping("/{discoveryHandlerId}")
    public ShenyuAdminResult updateDiscoveryUpstream(@PathVariable("discoveryHandlerId")
                                                     @Existed(message = "discovery upstream is not existed",
                                                             provider = DiscoveryHandlerMapper.class) final String discoveryHandlerId,
                                                     @Valid @RequestBody final List<DiscoveryUpstreamDTO> discoveryUpstreamDTO) {
        return ShenyuAdminResult.success(discoveryUpstreamService.updateBatch(discoveryHandlerId, discoveryUpstreamDTO));
    }

    /**
     * batch delete.
     *
     * @param ids id list
     * @return {@linkplain ShenyuAdminResult}
     */
    @DeleteMapping("/batch")
    public ShenyuAdminResult deleteDiscoveryUpstream(@NotEmpty @RequestBody final List<@NotBlank String> ids) {
        return ShenyuAdminResult.success(discoveryUpstreamService.delete(ids));
    }
}
