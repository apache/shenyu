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

import org.apache.shenyu.admin.model.dto.ShenyuDictDTO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.query.ShenyuDictQuery;
import org.apache.shenyu.admin.model.vo.ShenyuDictVO;

import java.util.List;

/**
 * this is shenyu dict service.
 */
public interface ShenyuDictService {
    /**
     * find page of shenyu dict by query.
     *
     * @param shenyuDictQuery {@linkplain ShenyuDictQuery}
     * @return {@link CommonPager}
     */
    CommonPager<ShenyuDictVO> listByPage(ShenyuDictQuery shenyuDictQuery);

    /**
     * create or update shenyu dict.
     *
     * @param shenyuDictDTO {@linkplain ShenyuDictDTO}
     * @return affected rows
     */
    Integer createOrUpdate(ShenyuDictDTO shenyuDictDTO);

    /**
     * find shenyu dict by id.
     *
     * @param id shenyu dict id.
     * @return {@linkplain ShenyuDictVO}
     */
    ShenyuDictVO findById(String id);

    /**
     * find shenyu dict by dict code and dict name.
     *
     * @param dictCode shenyu dict code.
     * @param dictName shenyu dict name.
     * @return {@linkplain ShenyuDictVO}
     */
    ShenyuDictVO findByDictCodeName(String dictCode, String dictName);

    /**
     * find shenyu dict list by dict type.
     *
     * @param type the shenyu dict type.
     * @return shenyu dict list.
     */
    List<ShenyuDictVO> list(String type);

    /**
     * delete shenyu dicts.
     *
     * @param ids ids to delete
     * @return The number of rows deleted
     */
    Integer deleteShenyuDicts(List<String> ids);

    /**
     * Enabled string.
     *
     * @param ids     the ids
     * @param enabled the enable
     * @return the effect rows
     */
    Integer enabled(List<String> ids, Boolean enabled);
}
