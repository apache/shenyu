/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * Contributor license agreements.See the NOTICE file distributed with
 * This work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * he License.You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.dromara.soul.dashboard.service;

import org.dromara.soul.dashboard.dto.AppAuthDTO;
import org.dromara.soul.dashboard.dto.BatchCommonDTO;
import org.dromara.soul.dashboard.query.AppAuthQuery;
import org.dromara.soul.dashboard.result.CommonPageResponse;
import org.dromara.soul.dashboard.result.CommonResponse;
import org.dromara.soul.dashboard.vo.AppAuthVO;

import java.util.List;

/**
 * this is application authority service.
 *
 * @author xiaoyu(Myth)
 */
public interface AppAuthService {

    /**
     * save or update application authority.
     *
     * @param appAuthDTO {@linkplain AppAuthDTO}
     * @return rows int
     */
    CommonResponse saveOrUpdate(AppAuthDTO appAuthDTO);

    /**
     * delete application authorities.
     *
     * @param ids primary key.
     * @return rows int
     */
    CommonResponse batchDelete(List<String> ids);

    /**
     * Batch enabled common result.
     *
     * @param batchCommonDTO the batch common dto
     * @return the common result
     */
    CommonResponse batchEnabled(BatchCommonDTO batchCommonDTO);

    /**
     * find application authority by id.
     *
     * @param id primary key.
     * @return {@linkplain AppAuthVO}
     */
    AppAuthVO findById(String id);

    /**
     * find page of application authority by query.
     *
     * @param appAuthQuery {@linkplain AppAuthQuery}
     * @return {@linkplain CommonPageResponse}
     */
    CommonPageResponse<AppAuthVO> listPageByQuery(AppAuthQuery appAuthQuery);


}
