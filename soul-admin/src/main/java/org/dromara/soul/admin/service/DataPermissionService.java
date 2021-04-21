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

package org.dromara.soul.admin.service;

import org.dromara.soul.admin.model.dto.DataPermissionDTO;
import org.dromara.soul.admin.model.entity.DataPermissionDO;
import org.dromara.soul.admin.model.query.DataPermissionQuery;
import org.dromara.soul.admin.model.vo.DataPermissionPageVO;

import java.util.List;

/**
 * data permission service.
 *
 * @author kaitoshy(plutokaito)
 */
public interface DataPermissionService {

    /**
     * get user data permissions via  user_id and data_type.
     * @param userId user_id
     * @return List of {@link DataPermissionDO}
     */
    List<DataPermissionDO> getUserDataPermission(String userId);

    /**
     * Delete rows.
     * @param userId user_id
     * @return effect rows int
     */
    int deleteByUserId(String userId);


    /**
     * delete rows via data id.
     * @param dataId rule id or selector id
     * @return effect rows count
     */
    int deleteByDataId(String dataId);

    /**
     * Create data permission.
     * @param dataPermissionDTO {@linkplain DataPermissionDTO}
     * @return int
     */
    int create(DataPermissionDTO dataPermissionDTO);


    /**
     * Delete data permission.
     * @param dataPermissionDTO {@linkplain DataPermissionDTO}
     * @return int
     */
    int delete(DataPermissionDTO dataPermissionDTO);

    /**
     * Query data permission.
     * @param dataPermissionQuery {@linkplain DataPermissionQuery}
     * @return {@link DataPermissionPageVO}
     */
    DataPermissionPageVO listByPage(DataPermissionQuery dataPermissionQuery);
}
