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

import org.apache.shenyu.admin.model.dto.SoulDictDTO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.query.SoulDictQuery;
import org.apache.shenyu.admin.model.vo.SoulDictVO;

import java.util.List;

/**
 * this is soul dict service.
 *
 * @author dengliming
 */
public interface SoulDictService {
    /**
     * find page of soul dict by query.
     *
     * @param soulDictQuery {@linkplain SoulDictQuery}
     * @return {@link CommonPager}
     */
    CommonPager<SoulDictVO> listByPage(SoulDictQuery soulDictQuery);

    /**
     * create or update soul dict.
     *
     * @param soulDictDTO {@linkplain SoulDictDTO}
     * @return affected rows
     */
    Integer createOrUpdate(SoulDictDTO soulDictDTO);

    /**
     * find soul dict by id.
     *
     * @param id soul dict id.
     * @return {@linkplain SoulDictVO}
     */
    SoulDictVO findById(String id);

    /**
     * find soul dict list by dict type.
     *
     * @param type the soul dict type.
     * @return soul dict list.
     */
    List<SoulDictVO> list(String type);

    /**
     * delete soul dicts.
     *
     * @param ids ids to delete
     * @return The number of rows deleted
     */
    Integer deleteSoulDicts(List<String> ids);

    /**
     * Enabled string.
     *
     * @param ids     the ids
     * @param enabled the enable
     * @return the effect rows
     */
    Integer enabled(List<String> ids, Boolean enabled);
}
