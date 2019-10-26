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

import org.dromara.soul.dashboard.dto.SelectorDTO;
import org.dromara.soul.dashboard.query.SelectorQuery;
import org.dromara.soul.dashboard.result.CommonPageResponse;
import org.dromara.soul.dashboard.result.CommonResponse;
import org.dromara.soul.dashboard.vo.SelectorVO;

import java.util.List;

/**
 * this is selector service.
 *
 * @author jiangxiaofeng(Nicholas)
 */
public interface SelectorService {

    /**
     * create or update selector.
     *
     * @param selectorDTO {@linkplain SelectorDTO}
     * @return rows int
     */
    CommonResponse saveOrUpdate(SelectorDTO selectorDTO);

    /**
     * Batch delete common result.
     *
     * @param ids the ids
     * @return the common result
     */
    CommonResponse batchDelete(List<String> ids);

    /**
     * find selector by id.
     *
     * @param id primary key.
     * @return {@linkplain SelectorVO}
     */
    SelectorVO findById(String id);

    /**
     * find page of selector by query.
     *
     * @param selectorQuery {@linkplain SelectorQuery}
     * @return {@linkplain CommonPageResponse}
     */
    CommonPageResponse<SelectorVO> listPageByQuery(SelectorQuery selectorQuery);


}
