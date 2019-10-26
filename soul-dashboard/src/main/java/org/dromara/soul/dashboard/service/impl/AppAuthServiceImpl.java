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

package org.dromara.soul.dashboard.service.impl;

import com.github.pagehelper.Page;
import com.github.pagehelper.PageHelper;
import org.dromara.soul.dashboard.dto.AppAuthDTO;
import org.dromara.soul.dashboard.dto.BatchCommonDTO;
import org.dromara.soul.dashboard.entity.AppAuthDO;
import org.dromara.soul.dashboard.mapper.AppAuthMapper;
import org.dromara.soul.dashboard.query.AppAuthQuery;
import org.dromara.soul.dashboard.result.CommonPageResponse;
import org.dromara.soul.dashboard.result.CommonResponse;
import org.dromara.soul.dashboard.service.AppAuthService;
import org.dromara.soul.dashboard.transfer.AppAuthTransfer;
import org.dromara.soul.dashboard.vo.AppAuthVO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * The type App auth service.
 *
 * @author xiaoyu
 */
@Service("appAuthService")
public class AppAuthServiceImpl implements AppAuthService {

    private final AppAuthMapper appAuthMapper;

    @Autowired(required = false)
    public AppAuthServiceImpl(AppAuthMapper appAuthMapper) {
        this.appAuthMapper = appAuthMapper;
    }

    @Override
    public CommonResponse saveOrUpdate(AppAuthDTO appAuthDTO) {
        return null;
    }

    @Override
    public CommonResponse batchDelete(List<String> ids) {
        int rows = appAuthMapper.batchDelete(ids);
        if (rows > 0) {
            return CommonResponse.success();
        }
        return CommonResponse.error("batchDelete AppAuth exception!");
    }

    @Override
    public CommonResponse batchEnabled(BatchCommonDTO batchCommonDTO) {
        int rows = appAuthMapper.batchEnable(batchCommonDTO.getIds(), batchCommonDTO.getEnabled());
        if (rows > 0) {
            return CommonResponse.success();
        }
        return CommonResponse.error("batchEnabled AppAuth exception!");
    }

    @Override
    public AppAuthVO findById(String id) {
        return AppAuthTransfer.INSTANCE.mapToVO(appAuthMapper.findById(id));
    }

    @Override
    public CommonPageResponse<AppAuthVO> listPageByQuery(AppAuthQuery appAuthQuery) {
        Page<AppAuthVO> page = PageHelper.startPage(appAuthQuery.getCurrentPage(), appAuthQuery.getPageSize());
        List<AppAuthDO> appAuthDOList = appAuthMapper.findListByQuery(appAuthQuery);
        final List<AppAuthVO> coinVOList = AppAuthTransfer.INSTANCE.mapToListVO(appAuthDOList);
        return CommonPageResponse.success(appAuthQuery.getCurrentPage(), appAuthQuery.getPageSize(), page.getTotal(), coinVOList);
    }
}
