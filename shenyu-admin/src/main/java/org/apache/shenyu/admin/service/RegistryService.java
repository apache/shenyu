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

import org.apache.shenyu.admin.model.dto.RegistryDTO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.query.RegistryQuery;
import org.apache.shenyu.admin.model.vo.RegistryVO;

import java.util.List;

public interface RegistryService {

    /**
     * Create or update registry.
     *
     * @param registryDTO the registry dto
     * @return the string
     */
    RegistryVO createOrUpdate(RegistryDTO registryDTO);

    /**
     * find page of registry by query.
     *
     * @param registryQuery {@linkplain RegistryQuery}
     * @return {@linkplain CommonPager}
     */
    CommonPager<RegistryVO> listByPage(RegistryQuery registryQuery);

    /**
     * delete by id.
     *
     * @param ids ids
     * @return msg
     */
    String delete(List<String> ids);

    /**
     * find registry by id.
     *
     * @param id id.
     * @return {@linkplain RegistryVO}
     */
    RegistryVO findById(String id);

    /**
     * find registry by registryId.
     *
     * @param registryId registryId.
     * @return {@linkplain RegistryVO}
     */
    RegistryVO findByRegistryId(String registryId);

    /**
     * find list of registry.
     *
     * @return {@linkplain List}
     */
    List<RegistryVO> listAll();
}
