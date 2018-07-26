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

package org.dromara.soul.admin.service;

import org.dromara.soul.admin.dto.PluginDTO;
import org.dromara.soul.admin.entity.PluginDO;
import org.dromara.soul.admin.page.CommonPager;
import org.dromara.soul.admin.query.PluginQuery;

/**
 * this is plugin service.
 *
 * @author jiangxiaofeng(programgeek @ 163.com)
 */
public interface PluginService {

    /**
     * save or update plugin.
     *
     * @param pluginDTO {@linkplain PluginDTO}
     * @return rows
     */
    int saveOrUpdate(PluginDTO pluginDTO);

    /**
     * enabled or disabled plugin.
     *
     * @param pluginDTO {@linkplain PluginDTO}
     * @return rows
     */
    int enabled(PluginDTO pluginDTO);

    /**
     * find plugin by id.
     *
     * @param id pk.
     * @return {@linkplain PluginDO}
     */
    PluginDO findById(String id);

    /**
     * find page of plugin by query.
     *
     * @param pluginQuery {@linkplain PluginQuery}
     * @return {@linkplain CommonPager}
     */
    CommonPager<PluginDO> listByPage(PluginQuery pluginQuery);
}
