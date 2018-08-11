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

import org.I0Itec.zkclient.ZkClient;
import org.apache.commons.lang3.StringUtils;
import org.dromara.soul.admin.dto.AppAuthDTO;
import org.dromara.soul.admin.entity.AppAuthDO;
import org.dromara.soul.admin.mapper.AppAuthMapper;
import org.dromara.soul.admin.page.CommonPager;
import org.dromara.soul.admin.page.PageParameter;
import org.dromara.soul.admin.query.AppAuthQuery;
import org.dromara.soul.admin.service.AppAuthService;
import org.dromara.soul.admin.vo.AppAuthVO;
import org.dromara.soul.common.constant.ZkPathConstants;
import org.dromara.soul.common.dto.zk.AppAuthZkDTO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.stream.Collectors;

/**
 * AppAuthServiceImpl.
 *
 * @author xiaoyu(Myth)
 */
@Service("appAuthService")
public class AppAuthServiceImpl implements AppAuthService {

    private final AppAuthMapper appAuthMapper;

    private final ZkClient zkClient;

    @Autowired(required = false)
    public AppAuthServiceImpl(final AppAuthMapper appAuthMapper, final ZkClient zkClient) {
        this.appAuthMapper = appAuthMapper;
        this.zkClient = zkClient;
    }

    /**
     * create or update application authority.
     *
     * @param appAuthDTO {@linkplain AppAuthDTO}
     * @return rows
     */
    @Override
    public int createOrUpdate(final AppAuthDTO appAuthDTO) {
        int appAuthCount;

        AppAuthDO appAuthDO = AppAuthDO.buildAppAuthDO(appAuthDTO);
        if (StringUtils.isEmpty(appAuthDTO.getId())) {
            appAuthCount = appAuthMapper.insertSelective(appAuthDO);
        } else {
            appAuthCount = appAuthMapper.updateSelective(appAuthDO);
        }

        String appAuthPath = ZkPathConstants.buildAppAuthPath(appAuthDO.getAppKey());
        if (!zkClient.exists(appAuthPath)) {
            zkClient.createPersistent(appAuthPath, true);
        }
        zkClient.writeData(appAuthPath, new AppAuthZkDTO(appAuthDO.getAppKey(), appAuthDO.getAppSecret(), appAuthDO.getEnabled()));
        return appAuthCount;
    }

    /**
     * delete application authorities.
     *
     * @param ids primary key.
     * @return rows
     */
    @Override
    public int delete(final List<String> ids) {
        int appAuthCount = 0;

        for (String id : ids) {
            AppAuthDO appAuthDO = appAuthMapper.selectById(id);
            appAuthCount += appAuthMapper.delete(id);

            String pluginPath = ZkPathConstants.buildAppAuthPath(appAuthDO.getAppKey());
            if (zkClient.exists(pluginPath)) {
                zkClient.delete(pluginPath);
            }
        }
        return appAuthCount;
    }

    /**
     * find application authority by id.
     *
     * @param id primary key.
     * @return {@linkplain AppAuthVO}
     */
    @Override
    public AppAuthVO findById(final String id) {
        return AppAuthVO.buildAppAuthVO(appAuthMapper.selectById(id));
    }

    /**
     * find page of application authority by query.
     *
     * @param appAuthQuery {@linkplain AppAuthQuery}
     * @return {@linkplain CommonPager}
     */
    @Override
    public CommonPager<AppAuthVO> listByPage(final AppAuthQuery appAuthQuery) {
        PageParameter pageParameter = appAuthQuery.getPageParameter();
        return new CommonPager<>(
                new PageParameter(pageParameter.getCurrentPage(), pageParameter.getPageSize(), appAuthMapper.countByQuery(appAuthQuery)),
                appAuthMapper.selectByQuery(appAuthQuery).stream()
                        .map(AppAuthVO::buildAppAuthVO)
                        .collect(Collectors.toList()));
    }
}
