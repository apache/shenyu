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

import org.apache.shenyu.admin.model.dto.ApiDTO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.query.ApiQuery;
import org.apache.shenyu.admin.model.vo.ApiVO;
import org.apache.shenyu.admin.model.vo.PluginVO;

import java.util.List;

/**
 * this is api service.
 */
public interface ApiService {

    /**
     * Create or update string.
     *
     * @param apiDTO the api dto
     * @return the string
     */
    String createOrUpdate(ApiDTO apiDTO);

    /**
     * Delete string.
     *
     * @param ids the ids
     * @return the string
     */
    String delete(List<String> ids);
    
    /**
     * find api by id.
     *
     * @param id pk.
     * @return {@linkplain PluginVO}
     */
    ApiVO findById(String id);


    /**
     * find page of api by query.
     *
     * @param apiQuery {@linkplain ApiQuery}
     * @return {@linkplain CommonPager}
     */
    CommonPager<ApiVO> listByPage(ApiQuery apiQuery);

    /**
     * deleteByApiPathHttpMethodRpcType.
     * @param apiPath apiPath
     * @param httpMethod httpMethod
     * @param rpcType rpcType
     * @return delete rows
     */
    int deleteByApiPathHttpMethodRpcType(String apiPath, Integer httpMethod, String rpcType);

    /**
     * offlineByContextPath.
     * @param contextPath context path
     * @return the string
     */
    String offlineByContextPath(String contextPath);
}
