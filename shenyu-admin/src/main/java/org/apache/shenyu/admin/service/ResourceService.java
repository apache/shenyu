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

import org.apache.shenyu.admin.model.dto.CreateResourceDTO;
import org.apache.shenyu.admin.model.dto.ResourceDTO;
import org.apache.shenyu.admin.model.entity.ResourceDO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.query.ResourceQuery;
import org.apache.shenyu.admin.model.vo.PermissionMenuVO.MenuInfo;
import org.apache.shenyu.admin.model.vo.ResourceVO;

import java.util.List;

/**
 * this is Resource Service.
 */
public interface ResourceService {
    
    /**
     * create Resources.
     *
     * @param resourceDOList list of {@linkplain ResourceDO}
     * @return rows int
     */
    int createResourceBatch(List<ResourceDO> resourceDOList);
    
    /**
     * create resource.
     *
     * @param createResourceDTO {@linkplain CreateResourceDTO}
     * @return rows int
     */
    int create(CreateResourceDTO createResourceDTO);
    
    /**
     * update resource.
     *
     * @param resourceDTO {@linkplain ResourceDTO}
     * @return rows int
     */
    int update(ResourceDTO resourceDTO);
    
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
     * find by title.
     *
     * @param title resource title
     * @return {@linkplain ResourceVO}
     */
    ResourceVO findByTitle(String title);
    
    /**
     * find by title.
     *
     * @param titles resource titles
     * @return {@linkplain ResourceVO}
     */
    List<ResourceVO> listByTitles(List<String> titles);
    
    /**
     * find page of resource by query.
     *
     * @param resourceQuery {@linkplain ResourceQuery}
     * @return {@linkplain CommonPager}
     */
    CommonPager<ResourceVO> listByPage(ResourceQuery resourceQuery);
    
    /**
     * get menu tree.
     *
     * @return {@linkplain List}
     */
    List<MenuInfo> getMenuTree();
    
    /**
     * get button by parent id.
     *
     * @param id resource id
     * @return {@linkplain List}
     */
    List<ResourceVO> findByParentId(String id);
    
}
