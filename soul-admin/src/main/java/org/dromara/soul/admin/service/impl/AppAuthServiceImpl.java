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

import org.apache.commons.lang3.StringUtils;
import org.dromara.soul.admin.dto.AppAuthDTO;
import org.dromara.soul.admin.entity.AppAuthDO;
import org.dromara.soul.admin.listener.DataChangedEvent;
import org.dromara.soul.admin.mapper.AppAuthMapper;
import org.dromara.soul.admin.page.CommonPager;
import org.dromara.soul.admin.page.PageParameter;
import org.dromara.soul.admin.query.AppAuthQuery;
import org.dromara.soul.admin.service.AppAuthService;
import org.dromara.soul.admin.vo.AppAuthVO;
import org.dromara.soul.common.dto.AppAuthData;
import org.dromara.soul.common.enums.ConfigGroupEnum;
import org.dromara.soul.common.enums.DataEventTypeEnum;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;

import java.util.Collections;
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

    private final ApplicationEventPublisher eventPublisher;

    @Autowired(required = false)
    public AppAuthServiceImpl(final AppAuthMapper appAuthMapper, final ApplicationEventPublisher eventPublisher) {
        this.appAuthMapper = appAuthMapper;
        this.eventPublisher = eventPublisher;
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
        DataEventTypeEnum eventType;
        if (StringUtils.isEmpty(appAuthDTO.getId())) {
            appAuthCount = appAuthMapper.insertSelective(appAuthDO);
            eventType = DataEventTypeEnum.CREATE;
        } else {
            appAuthCount = appAuthMapper.updateSelective(appAuthDO);
            eventType = DataEventTypeEnum.UPDATE;
        }

        // publish AppAuthData's event
        AppAuthData data = new AppAuthData(appAuthDO.getAppKey(), appAuthDO.getAppSecret(), appAuthDO.getEnabled());
        eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.APP_AUTH, eventType, Collections.singletonList(data)));

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

            // publish delete event of AppAuthData
            AppAuthData data = new AppAuthData(appAuthDO.getAppKey(), appAuthDO.getAppSecret(), appAuthDO.getEnabled());
            eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.APP_AUTH, DataEventTypeEnum.DELETE, Collections.singletonList(data)));
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

    @Override
    public List<AppAuthData> listAll() {
        return appAuthMapper.selectAll()
                .stream()
                .map(appAuthDO -> new AppAuthData(appAuthDO.getAppKey(), appAuthDO.getAppSecret(), appAuthDO.getEnabled()))
                .collect(Collectors.toList());
    }
}
