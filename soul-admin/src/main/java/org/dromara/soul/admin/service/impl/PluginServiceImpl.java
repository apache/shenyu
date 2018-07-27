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

import org.dromara.soul.admin.dto.PluginDTO;
import org.dromara.soul.admin.entity.PluginDO;
import org.dromara.soul.admin.mapper.PluginMapper;
import org.dromara.soul.admin.page.CommonPager;
import org.dromara.soul.admin.query.PluginQuery;
import org.dromara.soul.admin.service.PluginService;
import org.dromara.soul.admin.vo.PluginVO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.stream.Collectors;

/**
 * PluginServiceImpl.
 *
 * @author jiangxiaofeng(Nicholas)
 */
@Service("pluginService")
public class PluginServiceImpl implements PluginService {

    private final PluginMapper pluginMapper;

    @Autowired(required = false)
    public PluginServiceImpl(final PluginMapper pluginMapper) {
        this.pluginMapper = pluginMapper;
    }

    /**
     * save or update plugin.
     *
     * @param pluginDTO {@linkplain PluginDTO}
     * @return rows
     */
    public int saveOrUpdate(final PluginDTO pluginDTO) {
        return 0;
    }

    /**
     * enabled or disabled plugin.
     *
     * @param pluginDTO {@linkplain PluginDTO}
     * @return rows
     */
    public int enabled(final PluginDTO pluginDTO) {
        return 0;
    }

    /**
     * find plugin by id.
     *
     * @param id pk.
     * @return {@linkplain PluginDO}
     */
    public PluginDO findById(final String id) {
        return null;
    }

    /**
     * find page of plugin by query.
     *
     * @param pluginQuery {@linkplain PluginQuery}
     * @return {@linkplain CommonPager}
     */
    public CommonPager<PluginVO> listByPage(final PluginQuery pluginQuery) {
        return new CommonPager<>(pluginQuery.getPageParameter(),
                pluginMapper.selectByQuery(pluginQuery).stream()
                        .map(PluginVO::buildPluginVO)
                        .collect(Collectors.toList()));
    }
}
