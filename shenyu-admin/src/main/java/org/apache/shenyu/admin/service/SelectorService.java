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

package org.apache.shenyu.admin.service;

import org.apache.shenyu.admin.model.dto.SelectorDTO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.query.SelectorQuery;
import org.apache.shenyu.admin.model.vo.SelectorVO;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;

import java.util.List;

/**
 * this is selector service.
 */
public interface SelectorService {


    /**
     * Register string.
     *
     * @param selectorDTO the selector dto
     * @return the string
     */
    String register(SelectorDTO selectorDTO);

    /**
     * create or update selector.
     *
     * @param selectorDTO {@linkplain SelectorDTO}
     * @return rows int
     */
    int createOrUpdate(SelectorDTO selectorDTO);

    /**
     * update selective selector.
     *
     * @param selectorDO {@linkplain SelectorDO}
     * @return rows int
     */
    int updateSelective(SelectorDO selectorDO);

    /**
     * delete selectors.
     *
     * @param ids primary key.
     * @return rows int
     */
    int delete(List<String> ids);

    /**
     * find selector by id.
     *
     * @param id primary key.
     * @return {@linkplain SelectorVO}
     */
    SelectorVO findById(String id);

    /**
     * find selector by name.
     *
     * @param name the name
     * @return selector do
     */
    SelectorDO findByName(String name);

    /**
     * Build by name selector data.
     *
     * @param name the name
     * @return the selector data
     */
    SelectorData buildByName(String name);

    /**
     * find page of selector by query.
     *
     * @param selectorQuery {@linkplain SelectorQuery}
     * @return {@linkplain CommonPager}
     */
    CommonPager<SelectorVO> listByPage(SelectorQuery selectorQuery);

    /**
     * Find by plugin id list.
     *
     * @param pluginId the plugin id
     * @return the list
     */
    List<SelectorData> findByPluginId(String pluginId);

    /**
     * List all list.
     *
     * @return the list
     */
    List<SelectorData> listAll();

    /**
     * handler selector need upstream check.
     *
     * @param dto      {@link MetaDataRegisterDTO}
     * @param rpcType  rpc type
     * @return the id of selector.
     */
    String handlerSelectorNeedUpstreamCheck(MetaDataRegisterDTO dto, String rpcType);
}
