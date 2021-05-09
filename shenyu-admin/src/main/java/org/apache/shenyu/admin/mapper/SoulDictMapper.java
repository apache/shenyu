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

package org.apache.shenyu.admin.mapper;

import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;
import org.apache.shenyu.admin.model.entity.ShenyuDictDO;
import org.apache.shenyu.admin.model.query.SoulDictQuery;

import java.util.List;

/**
 * The soul dict mapper.
 *
 * @author dengliming
 */
@Mapper
public interface SoulDictMapper {

    /**
     * Select soul dict by id.
     *
     * @param id the id.
     * @return the soul dict do.
     */
    ShenyuDictDO selectById(@Param("id") String id);

    /**
     * find soul dict do list by dict type.
     *
     * @param type the dict type.
     * @return the list
     */
    List<ShenyuDictDO> findByType(@Param("type") String type);

    /**
     * insert soul dict.
     *
     * @param record {@link ShenyuDictDO}
     * @return affected rows
     */
    int insert(ShenyuDictDO record);

    /**
     * insert selective soul dict.
     *
     * @param record {@link ShenyuDictDO}
     * @return affected rows.
     */
    int insertSelective(ShenyuDictDO record);

    /**
     * count soul dict by query.
     *
     * @param soulDictQuery {@linkplain SoulDictQuery}
     * @return the count
     */
    int countByQuery(SoulDictQuery soulDictQuery);

    /**
     * select soul dict list by query.
     *
     * @param soulDictQuery {@linkplain SoulDictQuery}
     * @return the soul dict list
     */
    List<ShenyuDictDO> selectByQuery(SoulDictQuery soulDictQuery);

    /**
     * update some selective columns in soul dict.
     *
     * @param record {@linkplain ShenyuDictDO}
     * @return affected rows
     */
    int updateByPrimaryKeySelective(ShenyuDictDO record);

    /**
     * update soul dict by primary key.
     *
     * @param record {@linkplain ShenyuDictDO}
     * @return effected rows.
     */
    int updateByPrimaryKey(ShenyuDictDO record);

    /**
     * delete string id.
     *
     * @param id soul dict id
     * @return affected rows
     */
    int delete(String id);

    /**
     * batch enabled.
     *
     * @param ids soul dict ids
     * @param enabled enabled status
     * @return affected rows
     */
    Integer enabled(@Param("ids") List<String> ids, @Param("enabled") Boolean enabled);
}
