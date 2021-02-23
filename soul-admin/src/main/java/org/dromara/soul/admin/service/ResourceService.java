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

import org.dromara.soul.admin.dto.ResourceDTO;
import org.dromara.soul.admin.page.CommonPager;
import org.dromara.soul.admin.query.ResourceQuery;
import org.dromara.soul.admin.vo.ResourceVO;

import java.util.List;

/**
 * this is Resource Service.
 *
 * @author nuo-promise
 */
public interface ResourceService {

    /**
     * create or update resource.
     *
     * @param resourceDTO {@linkplain ResourceDTO}
     * @return rows int
     */
    int createOrUpdate(ResourceDTO resourceDTO);

    /**
     * delete resource by id.
     *
     * @param ids {@linkplain List}
     * @return rows int
     */
    int delete(List<String> ids);

    /**
     * find by id.
     *
     * @param id resource id
     * @return {@linkplain ResourceVO}
     */
    ResourceVO findById(String id);

    /**
     * find page of resource by query.
     *
     * @param resourceQuery {@linkplain ResourceQuery}
     * @return {@linkplain CommonPager}
     */
    CommonPager<ResourceVO> listByPage(ResourceQuery resourceQuery);
}
