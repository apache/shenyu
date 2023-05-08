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

package org.apache.shenyu.admin.service.impl;

import org.apache.shenyu.admin.mapper.DiscoveryUpstreamMapper;
import org.apache.shenyu.admin.model.dto.DiscoveryUpstreamDTO;
import org.apache.shenyu.admin.model.entity.DiscoveryUpstreamDO;
import org.apache.shenyu.admin.service.DiscoveryUpstreamService;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;

import java.util.List;

@Service
public class DiscoveryUpstreamServiceImpl implements DiscoveryUpstreamService {

    private final DiscoveryUpstreamMapper discoveryUpstreamMapper;

    public DiscoveryUpstreamServiceImpl(final DiscoveryUpstreamMapper discoveryUpstreamMapper) {

        this.discoveryUpstreamMapper = discoveryUpstreamMapper;
    }

    /**
     * createOrUpdate.
     *
     * @param discoveryUpstreamDTO discoveryUpstreamDTO
     * @return the string
     */
    @Override
    public String createOrUpdate(final DiscoveryUpstreamDTO discoveryUpstreamDTO) {

        return StringUtils.hasLength(discoveryUpstreamDTO.getId())
                ? update(discoveryUpstreamDTO) : create(discoveryUpstreamDTO);
    }

    /**
     * delete.
     *
     * @param ids id list
     * @return the string
     */
    @Override
    public String delete(final List<String> ids) {

        discoveryUpstreamMapper.deleteByIds(ids);
        return ShenyuResultMessage.DELETE_SUCCESS;
    }

    /**
     * create.
     *
     * @param discoveryUpstreamDTO discoveryUpstreamDTO
     * @return the string
     */
    private String create(final DiscoveryUpstreamDTO discoveryUpstreamDTO) {

        DiscoveryUpstreamDO discoveryUpstreamDO = DiscoveryUpstreamDO.buildDiscoveryUpstreamDO(discoveryUpstreamDTO);
        discoveryUpstreamMapper.insert(discoveryUpstreamDO);
        return ShenyuResultMessage.CREATE_SUCCESS;
    }

    /**
     * update.
     *
     * @param discoveryUpstreamDTO discoveryUpstreamDTO
     * @return the string
     */
    private String update(final DiscoveryUpstreamDTO discoveryUpstreamDTO) {

        DiscoveryUpstreamDO discoveryUpstreamDO = DiscoveryUpstreamDO.buildDiscoveryUpstreamDO(discoveryUpstreamDTO);
        discoveryUpstreamMapper.update(discoveryUpstreamDO);
        return ShenyuResultMessage.UPDATE_SUCCESS;
    }
}
