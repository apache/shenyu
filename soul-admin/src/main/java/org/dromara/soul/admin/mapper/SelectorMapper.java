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
import org.dromara.soul.admin.entity.SelectorDO;

/**
 * SelectorMapper.
 *
 * @author jiangxiaofeng(Nicholas)
 */
@Mapper
public interface SelectorMapper {

    /**
     * select selector by id.
     *
     * @param id pk
     * @return {@linkplain SelectorDO}
     */
    SelectorDO selectById(String id);

    /**
     * insert selector.
     *
     * @param selectorDO {@linkplain SelectorDO}
     * @return rows
     */
    int insert(SelectorDO selectorDO);

    /**
     * insert selective selector.
     *
     * @param selectorDO {@linkplain SelectorDO}
     * @return rows
     */
    int insertSelective(SelectorDO selectorDO);

    /**
     * update selector.
     *
     * @param selectorDO {@linkplain SelectorDO}
     * @return rows
     */
    int update(SelectorDO selectorDO);

    /**
     * update selective selector.
     *
     * @param selectorDO {@linkplain SelectorDO}
     * @return rows
     */
    int updateSelective(SelectorDO selectorDO);
}
