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

import org.apache.shenyu.admin.model.dto.DetailDTO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.query.DetailQuery;
import org.apache.shenyu.admin.model.vo.DetailVO;

import java.util.List;

public interface DetailService {

    /**
     * Create or update string.
     *
     * @param apiDTO the api dto
     * @return the string
     */
    int createOrUpdate(DetailDTO apiDTO);

    /**
     * Delete by id.
     *
     * @param id the id
     * @return the string
     */
    int delete(String id);

    /**
     * deleteBatch by ids.
     * @param ids ids.
     * @return int
     */
    int deleteBatch(List<String> ids);

    /**
     * find api by id.
     *
     * @param id pk.
     * @return {@linkplain DetailVO}
     */
    DetailVO findById(String id);

    /**
     * find page of api by query.
     *
     * @param apiQuery {@linkplain DetailQuery}
     * @return {@linkplain CommonPager}
     */
    CommonPager<DetailVO> listByPage(DetailQuery apiQuery);

}
