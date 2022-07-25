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

import org.apache.shenyu.admin.model.dto.AppAuthDTO;
import org.apache.shenyu.admin.model.dto.AuthApplyDTO;
import org.apache.shenyu.admin.model.dto.AuthPathWarpDTO;
import org.apache.shenyu.admin.model.entity.AppAuthDO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.query.AppAuthQuery;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.model.vo.AppAuthVO;
import org.apache.shenyu.admin.model.vo.AuthPathVO;
import org.apache.shenyu.common.dto.AppAuthData;

import java.util.List;

/**
 * this is application authority service.
 */
public interface AppAuthService extends PageService<AppAuthQuery, AppAuthVO> {


    /**
     * Apply create shenyu result.
     *
     * @param authApplyDTO the auth apply dto
     * @return the shenyu result
     */
    ShenyuAdminResult applyCreate(AuthApplyDTO authApplyDTO);


    /**
     * Apply update shenyu result.
     *
     * @param authApplyDTO the auth apply dto
     * @return the shenyu result
     */
    ShenyuAdminResult applyUpdate(AuthApplyDTO authApplyDTO);


    /**
     * Update detail shenyu result.
     *
     * @param appAuthDTO the app auth dto
     * @return the shenyu result
     */
    ShenyuAdminResult updateDetail(AppAuthDTO appAuthDTO);

    /**
     * Update detail path shenyu result.
     *
     * @param authPathWarpDTO the auth path warp dto
     * @return the shenyu result
     */
    ShenyuAdminResult updateDetailPath(AuthPathWarpDTO authPathWarpDTO);

    /**
     * create or update application authority.
     *
     * @param appAuthDTO {@linkplain AppAuthDTO}
     * @return rows int
     */
    int createOrUpdate(AppAuthDTO appAuthDTO);

    /**
     * delete application authorities.
     *
     * @param ids primary key.
     * @return rows int
     */
    int delete(List<String> ids);


    /**
     * Enabled string.
     *
     * @param ids     the ids
     * @param enabled the enable
     * @return the string
     */
    String enabled(List<String> ids, Boolean enabled);

    /**
     * find application authority by id.
     *
     * @param id primary key.
     * @return {@linkplain AppAuthVO}
     */
    AppAuthVO findById(String id);


    /**
     * Detail path auth path vo.
     *
     * @param id the id
     * @return the auth path vo
     */
    List<AuthPathVO> detailPath(String id);

    /**
     * find page of application authority by query.
     *
     * @param appAuthQuery {@linkplain AppAuthQuery}
     * @return {@linkplain CommonPager}
     */
    CommonPager<AppAuthVO> listByPage(AppAuthQuery appAuthQuery);

    /**
     * List all list.
     *
     * @return the list
     */
    List<AppAuthData> listAll();

    /**
     * Update app secret by app key shenyu result.
     *
     * @param appKey    the app key
     * @param appSecret the app secret
     * @return the shenyu result
     */
    ShenyuAdminResult updateAppSecretByAppKey(String appKey, String appSecret);

    /**
     * Find by app key app auth do.
     *
     * @param appKey the app key
     * @return the app auth do
     */
    AppAuthDO findByAppKey(String appKey);


    /**
     * Sync data shenyu result.
     *
     * @return the shenyu result
     */
    ShenyuAdminResult syncData();

}
