/*
 *   Licensed to the Apache Software Foundation (final ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (final the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 *
 */

package org.dromara.soul.admin.service.impl;

import org.I0Itec.zkclient.ZkClient;
import org.apache.commons.lang3.StringUtils;
import org.dromara.soul.admin.dto.PluginDTO;
import org.dromara.soul.admin.entity.PluginDO;
import org.dromara.soul.admin.mapper.PluginMapper;
import org.dromara.soul.admin.page.CommonPager;
import org.dromara.soul.admin.page.PageParameter;
import org.dromara.soul.admin.query.PluginQuery;
import org.dromara.soul.admin.service.PluginService;
import org.dromara.soul.admin.vo.PluginVO;
import org.dromara.soul.common.constant.ZkPathConstants;
import org.dromara.soul.common.dto.zk.PluginZkDTO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.stream.Collectors;

/**
 * PluginServiceImpl.
 *
 * @author jiangxiaofeng(Nicholas)
 */
@Service("pluginService")
public class PluginServiceImpl implements PluginService {

    private final PluginMapper pluginMapper;

    private final ZkClient zkClient;

    @Autowired(required = false)
    public PluginServiceImpl(final PluginMapper pluginMapper, final ZkClient zkClient) {
        this.pluginMapper = pluginMapper;
        this.zkClient = zkClient;
    }

    /**
     * create or update plugin.
     *
     * @param pluginDTO {@linkplain PluginDTO}
     * @return rows
     */
    public int createOrUpdate(final PluginDTO pluginDTO) {
        int pluginCount;

        PluginDO pluginDO = PluginDO.buildPluginDO(pluginDTO);
        if (StringUtils.isEmpty(pluginDTO.getId())) {
            pluginCount = pluginMapper.insertSelective(pluginDO);
        } else {
            pluginCount = pluginMapper.updateSelective(pluginDO);
        }

        String pluginPath = ZkPathConstants.buildPluginPath(pluginDO.getName());
        if (!zkClient.exists(pluginPath)) {
            zkClient.createPersistent(pluginPath, true);
        }
        zkClient.writeData(pluginPath, new PluginZkDTO(pluginDO.getId(), pluginDO.getName(), pluginDO.getEnabled()));
        return pluginCount;
    }

    /**
     * delete plugins.
     *
     * @param ids primary key.
     * @return rows
     */
    public int delete(final List<String> ids) {
        int pluginCount = 0;

        for (String id : ids) {
            PluginDO pluginDO = pluginMapper.selectById(id);
            pluginCount += pluginMapper.delete(id);

            String pluginPath = ZkPathConstants.buildPluginPath(pluginDO.getName());
            if (zkClient.exists(pluginPath)) {
                zkClient.delete(pluginPath);
            }
            String selectorParentPath = ZkPathConstants.buildSelectorParentPath(pluginDO.getName());
            if (zkClient.exists(selectorParentPath)) {
                zkClient.delete(selectorParentPath);
            }
            String ruleParentPath = ZkPathConstants.buildRuleParentPath(pluginDO.getName());
            if (zkClient.exists(ruleParentPath)) {
                zkClient.delete(ruleParentPath);
            }
        }
        return pluginCount;
    }

    /**
     * find plugin by id.
     *
     * @param id primary key.
     * @return {@linkplain PluginVO}
     */
    public PluginVO findById(final String id) {
        return PluginVO.buildPluginVO(pluginMapper.selectById(id));
    }

    /**
     * find page of plugin by query.
     *
     * @param pluginQuery {@linkplain PluginQuery}
     * @return {@linkplain CommonPager}
     */
    public CommonPager<PluginVO> listByPage(final PluginQuery pluginQuery) {
        PageParameter pageParameter = pluginQuery.getPageParameter();
        return new CommonPager<>(
                new PageParameter(pageParameter.getCurrentPage(), pageParameter.getPageSize(), pluginMapper.countByQuery(pluginQuery)),
                pluginMapper.selectByQuery(pluginQuery).stream()
                        .map(PluginVO::buildPluginVO)
                        .collect(Collectors.toList()));
    }
}
