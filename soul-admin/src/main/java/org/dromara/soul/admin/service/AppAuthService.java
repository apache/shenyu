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

package org.dromara.soul.admin.service;

import org.dromara.soul.admin.dto.AppAuthDTO;
import org.dromara.soul.admin.dto.AuthApplyDTO;
import org.dromara.soul.admin.dto.AuthPathWarpDTO;
import org.dromara.soul.admin.page.CommonPager;
import org.dromara.soul.admin.query.AppAuthQuery;
import org.dromara.soul.admin.result.SoulAdminResult;
import org.dromara.soul.admin.vo.AppAuthVO;
import org.dromara.soul.admin.vo.AuthPathVO;
import org.dromara.soul.common.dto.AppAuthData;

import java.util.List;

/**
 * this is application authority service.
 *
 * @author xiaoyu(Myth)
 */
public interface AppAuthService {


    /**
     * Apply create soul result.
     *
     * @param authApplyDTO the auth apply dto
     * @return the soul result
     */
    SoulAdminResult applyCreate(AuthApplyDTO authApplyDTO);


    /**
     * Apply update soul result.
     *
     * @param authApplyDTO the auth apply dto
     * @return the soul result
     */
    SoulAdminResult applyUpdate(AuthApplyDTO authApplyDTO);


    /**
     * Update detail soul result.
     *
     * @param appAuthDTO the app auth dto
     * @return the soul result
     */
    SoulAdminResult updateDetail(AppAuthDTO appAuthDTO);

    /**
     * Update detail path soul result.
     *
     * @param authPathWarpDTO the auth path warp dto
     * @return the soul result
     */
    SoulAdminResult updateDetailPath(AuthPathWarpDTO authPathWarpDTO);

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
     * Update app secret by app key soul result.
     *
     * @param appKey    the app key
     * @param appSecret the app secret
     * @return the soul result
     */
    SoulAdminResult updateAppSecretByAppKey(String appKey, String appSecret);


    /**
     * Sync data soul result.
     *
     * @return the soul result
     */
    SoulAdminResult syncData();

}
