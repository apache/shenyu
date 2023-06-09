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

import java.util.List;
import org.apache.shenyu.admin.model.dto.MockRequestRecordDTO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.query.MockRequestRecordQuery;
import org.apache.shenyu.admin.model.vo.MockRequestRecordVO;
import org.apache.shenyu.admin.model.vo.PluginVO;

/**
 * this is mock request record service.
 */
public interface MockRequestRecordService {

    /**
     *  createOrUpdate.
     * @param mockRequestRecordDTO mockRequestRecordDTO
     * @return rows
     */
    int createOrUpdate(MockRequestRecordDTO mockRequestRecordDTO);

    /**
     * Delete string.
     *
     * @param id the key
     * @return the string
     */
    int delete(String id);

    /**
     * batch delete.
     * @param ids ids
     * @return delete rows
     */
    int batchDelete(List<String> ids);

    /**
     * find mockrequestrecord by id.
     *
     * @param id pk.
     * @return {@linkplain PluginVO}
     */
    MockRequestRecordVO findById(String id);


    /**
     * find page of api by query.
     *
     * @param mockRequestRecordQuery {@linkplain MockRequestRecordQuery}
     * @return {@linkplain CommonPager}
     */
    CommonPager<MockRequestRecordVO> listByPage(MockRequestRecordQuery mockRequestRecordQuery);

    /**
     * find mock request by api id.
     *
     * @param apiId the api id.
     * @return {@linkplain MockRequestRecordVO}
     */
    MockRequestRecordVO queryByApiId(String apiId);
}
