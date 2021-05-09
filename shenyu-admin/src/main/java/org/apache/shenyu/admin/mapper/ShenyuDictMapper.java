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
import org.apache.shenyu.admin.model.query.ShenyuDictQuery;

import java.util.List;

/**
 * The shenyu dict mapper.
 */
@Mapper
public interface ShenyuDictMapper {

    /**
     * Select shenyu dict by id.
     *
     * @param id the id.
     * @return the shenyu dict do.
     */
    ShenyuDictDO selectById(@Param("id") String id);

    /**
     * find shenyu dict do list by dict type.
     *
     * @param type the dict type.
     * @return the list
     */
    List<ShenyuDictDO> findByType(@Param("type") String type);

    /**
     * insert shenyu dict.
     *
     * @param record {@link ShenyuDictDO}
     * @return affected rows
     */
    int insert(ShenyuDictDO record);

    /**
     * insert selective shenyu dict.
     *
     * @param record {@link ShenyuDictDO}
     * @return affected rows.
     */
    int insertSelective(ShenyuDictDO record);

    /**
     * count shenyu dict by query.
     *
     * @param shenyuDictQuery {@linkplain ShenyuDictQuery}
     * @return the count
     */
    int countByQuery(ShenyuDictQuery shenyuDictQuery);

    /**
     * select shenyu dict list by query.
     *
     * @param shenyuDictQuery {@linkplain ShenyuDictQuery}
     * @return the shenyu dict list
     */
    List<ShenyuDictDO> selectByQuery(ShenyuDictQuery shenyuDictQuery);

    /**
     * update some selective columns in shenyu dict.
     *
     * @param record {@linkplain ShenyuDictDO}
     * @return affected rows
     */
    int updateByPrimaryKeySelective(ShenyuDictDO record);

    /**
     * update shenyu dict by primary key.
     *
     * @param record {@linkplain ShenyuDictDO}
     * @return effected rows.
     */
    int updateByPrimaryKey(ShenyuDictDO record);

    /**
     * delete string id.
     *
     * @param id shenyu dict id
     * @return affected rows
     */
    int delete(String id);

    /**
     * batch enabled.
     *
     * @param ids shenyu dict ids
     * @param enabled enabled status
     * @return affected rows
     */
    Integer enabled(@Param("ids") List<String> ids, @Param("enabled") Boolean enabled);
}
