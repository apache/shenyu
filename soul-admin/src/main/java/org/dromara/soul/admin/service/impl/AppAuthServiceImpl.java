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

package org.dromara.soul.admin.service.impl;

import org.dromara.soul.admin.dto.AppAuthDTO;
import org.dromara.soul.admin.entity.AppAuthDO;
import org.dromara.soul.admin.page.CommonPager;
import org.dromara.soul.admin.query.AppAuthQuery;
import org.dromara.soul.admin.service.AppAuthService;
import org.dromara.soul.admin.vo.AppAuthVO;
import org.springframework.stereotype.Service;

/**
 * AppAuthServiceImpl.
 *
 * @author xiaoyu(549477611 @ qq.com)
 */
@Service("appAuthService")
public class AppAuthServiceImpl implements AppAuthService {
    @Override
    public int saveOrUpdate(final AppAuthDTO appAuthDTO) {
        return 0;
    }

    @Override
    public int enabled(final AppAuthDTO appAuthDTO) {
        return 0;
    }

    @Override
    public AppAuthDO findById(final String id) {
        return null;
    }

    @Override
    public AppAuthDO findByAppKey(final String appKey) {
        return null;
    }

    @Override
    public CommonPager<AppAuthVO> listByPage(final AppAuthQuery appAuthQuery) {
        return null;
    }
}
