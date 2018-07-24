/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 *
 */

package org.dromara.soul.admin.service;

import org.dromara.soul.admin.dto.AppAuthDTO;
import org.dromara.soul.admin.entity.AppAuthDO;
import org.dromara.soul.admin.page.CommonPager;
import org.dromara.soul.admin.query.AppAuthQuery;
import org.dromara.soul.admin.vo.AppAuthVO;

/**
 * this is application authority service.
 *
 * @author xiaoyu(549477611 @ qq.com)
 */
public interface AppAuthService {

    /**
     * save or update application authority.
     *
     * @param appAuthDTO {@linkplain AppAuthDTO}
     * @return rows
     */
    int saveOrUpdate(AppAuthDTO appAuthDTO);

    /**
     * enabled or disabled application authority.
     *
     * @param appAuthDTO {@linkplain AppAuthDTO}
     * @return rows
     */
    int enabled(AppAuthDTO appAuthDTO);

    /**
     * find application authority by id.
     *
     * @param id pk.
     * @return {@linkplain AppAuthDO}
     */
    AppAuthDO findById(String id);

    /**
     * find application authority by appKey.
     *
     * @param appKey appKey.
     * @return {@linkplain AppAuthDO}
     */
    AppAuthDO findByAppKey(String appKey);

    /**
     * find page of application authority by query.
     *
     * @param appAuthQuery {@linkplain AppAuthQuery}
     * @return CommonPager<AppAuthVO>
     */
    CommonPager<AppAuthVO> listByPage(AppAuthQuery appAuthQuery);
}