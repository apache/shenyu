/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
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

package org.dromara.soul.admin.mapper;

import org.apache.ibatis.annotations.Mapper;
import org.dromara.soul.admin.entity.PluginDO;

/**
 * PluginMapper.
 *
 * @author jiangxiaofeng(Nicholas)
 */
@Mapper
public interface PluginMapper {

    /**
     * select plugin by id.
     *
     * @param id pk
     * @return {@linkplain PluginDO}
     */
    PluginDO selectById(String id);

    /**
     * insert plugin.
     *
     * @param pluginDO {@linkplain PluginDO}
     * @return rows
     */
    int insert(PluginDO pluginDO);

    /**
     * insert selective plugin.
     *
     * @param pluginDO {@linkplain PluginDO}
     * @return rows
     */
    int insertSelective(PluginDO pluginDO);

    /**
     * update plugin.
     *
     * @param pluginDO {@linkplain PluginDO}
     * @return rows
     */
    int update(PluginDO pluginDO);

    /**
     * update selective plugin.
     *
     * @param pluginDO {@linkplain PluginDO}
     * @return rows
     */
    int updateSelective(PluginDO pluginDO);
}
