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
import org.apache.shenyu.admin.model.dto.AlertWebhookDTO;
import org.apache.shenyu.admin.model.entity.AlertWebhookDO;

import java.util.List;

@Mapper
public interface AlertWebhookMapper {

    /**
     * delete by id.
     * @param id id
     * @return rows int.
     */
    int deleteByPrimaryKey(String id);

    /**
     * insert selective alertWebhook.
     * @param alertWebhookDTO alertWebhook
     * @return rows int
     */
    int insertSelective(AlertWebhookDTO alertWebhookDTO);

    /**
     * select by id.
     * @param id id
     * @return AlertWebhookDO
     */
    AlertWebhookDO selectByPrimaryKey(String id);

    /**
     * update selective by id.
     * @param alertWebhookDTO alertWebhook
     * @return rows int
     */
    int updateByPrimaryKeySelective(AlertWebhookDTO alertWebhookDTO);

    /**
     * select all.
     * @return AlertWebhookDO of list
     */
    List<AlertWebhookDO> getAll();

}
