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
import org.apache.shenyu.admin.model.entity.AlertReceiverDO;
import org.apache.shenyu.admin.model.query.AlertReceiverQuery;
import org.apache.shenyu.admin.validation.ExistProvider;
import org.apache.shenyu.alert.model.AlertReceiverDTO;

import java.io.Serializable;
import java.util.List;

/**
 * AlertReceiverMapper.
 **/
@Mapper
public interface AlertReceiverMapper extends ExistProvider {
    
    
    /**
     * select all receiver.
     *
     * @return receiver list
     */
    List<AlertReceiverDTO> selectAll();
    
    /**
     * insert record to table.
     *
     * @param record the record
     * @return insert count
     */
    int insert(AlertReceiverDO record);
    
    /**
     * select by primary key.
     *
     * @param id primary key
     * @return object by primary key
     */
    AlertReceiverDO selectByPrimaryKey(String id);
    
    /**
     * update record.
     *
     * @param record the updated record
     * @return update count
     */
    int updateByPrimaryKey(AlertReceiverDO record);
    
    /**
     * existed.
     *
     * @param id id
     * @return existed
     */
    @Override
    Boolean existed(@Param("id") Serializable id);
    
    /**
     * delete api.
     *
     * @param ids primary keys.
     * @return rows int
     */
    int deleteByIds(List<String> ids);
    
    /**
     * select receivers by query.
     *
     * @param receiverQuery {@linkplain AlertReceiverQuery}
     * @return {@linkplain List}
     */
    List<AlertReceiverDO> selectByQuery(AlertReceiverQuery receiverQuery);
}
