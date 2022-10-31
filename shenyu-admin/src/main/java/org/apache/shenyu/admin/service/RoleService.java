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

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.model.dto.RoleDTO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.query.RoleQuery;
import org.apache.shenyu.admin.model.vo.RoleEditVO;
import org.apache.shenyu.admin.model.vo.RoleVO;

import java.util.List;

/**
 * this is role service.
 */
public interface RoleService {
    
    /**
     * create or update rule.
     *
     * @param roleDTO {@linkplain RoleDTO}
     * @return rows int
     */
    default int createOrUpdate(RoleDTO roleDTO) {
        return StringUtils.isBlank(roleDTO.getId()) ? create(roleDTO) : update(roleDTO);
    }
    
    /**
     * create or update rule.
     *
     * @param roleDTO {@linkplain RoleDTO}
     * @return rows int
     */
    int create(RoleDTO roleDTO);
    
    /**
     * create or update rule.
     *
     * @param roleDTO {@linkplain RoleDTO}
     * @return rows int
     */
    int update(RoleDTO roleDTO);
    
    /**
     * delete roles.
     *
     * @param ids primary key
     * @return rows int
     */
    int delete(List<String> ids);
    
    /**
     * find role by id.
     *
     * @param id primary key
     * @return {@linkplain RoleEditVO}
     */
    RoleEditVO findById(String id);
    
    /**
     * find role by roleName.
     *
     * @param roleName role name
     * @return {@linkplain RoleVO}
     */
    RoleVO findByQuery(String roleName);
    
    /**
     * find page of role by query.
     *
     * @param roleQuery {@linkplain RoleQuery}
     * @return {@linkplain CommonPager}
     */
    CommonPager<RoleVO> listByPage(RoleQuery roleQuery);
    
    /**
     * select all roles not super.
     *
     * @return {@linkplain List}
     */
    List<RoleVO> selectAll();
}
