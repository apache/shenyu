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

package org.apache.shenyu.admin.service.impl;

import com.google.common.collect.Lists;
import org.apache.shenyu.admin.exception.ShenyuAdminException;
import org.apache.shenyu.admin.mapper.NamespaceUserRelMapper;
import org.apache.shenyu.admin.model.entity.DashboardUserDO;
import org.apache.shenyu.admin.model.entity.NamespaceDO;
import org.apache.shenyu.admin.model.entity.NamespaceUserRelDO;
import org.apache.shenyu.admin.model.event.namespace.NamespaceCreatedEvent;
import org.apache.shenyu.admin.model.event.user.UserCreatedEvent;
import org.apache.shenyu.admin.model.vo.NamespaceUserRelVO;
import org.apache.shenyu.admin.service.NamespaceUserService;
import org.apache.shenyu.common.constant.AdminConstants;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

@Service
public class NamespaceUserServiceImpl implements NamespaceUserService {
    
    private final NamespaceUserRelMapper namespaceUserRelMapper;
    
    public NamespaceUserServiceImpl(final NamespaceUserRelMapper namespaceUserRelMapper) {
        this.namespaceUserRelMapper = namespaceUserRelMapper;
    }
    
    @Override
    public NamespaceUserRelVO create(final String namespaceId, final String userId) {
        NamespaceUserRelDO existNamespaceUserRelDO = namespaceUserRelMapper.selectByNamespaceIdAndUserId(namespaceId, userId);
        if (!Objects.isNull(existNamespaceUserRelDO)) {
            throw new ShenyuAdminException(AdminConstants.NAMESPACE_USER_EXIST);
        }
        String uuid = UUIDUtils.getInstance().generateShortUuid();
        NamespaceUserRelDO namespaceUserRelDO = NamespaceUserRelDO.builder()
                .id(uuid)
                .namespaceId(namespaceId)
                .userId(userId)
                .build();
        namespaceUserRelMapper.insertSelective(namespaceUserRelDO);
        
        return NamespaceUserRelVO.builder()
                .id(uuid)
                .namespaceId(namespaceId)
                .userId(userId)
                .build();
    }
    
    @Override
    public List<String> listNamespaceIdByUserId(final String userId) {
        List<NamespaceUserRelDO> namespaceUserRelDOS = namespaceUserRelMapper.selectListByUserId(userId);
        if (Objects.isNull(namespaceUserRelDOS)) {
            return Lists.newArrayList();
        }
        return namespaceUserRelDOS.stream().map(NamespaceUserRelDO::getNamespaceId).collect(Collectors.toList());
    }
    
    @EventListener(value = UserCreatedEvent.class)
    public void onUserCreated(final UserCreatedEvent event) {
        DashboardUserDO dashboardUserDO = event.getChangedUser();
        if (Objects.isNull(dashboardUserDO)) {
            return;
        }
        create(Constants.SYS_DEFAULT_NAMESPACE_ID, dashboardUserDO.getId());
    }
    
    @EventListener(value = NamespaceCreatedEvent.class)
    public void onNamespaceCreated(final NamespaceCreatedEvent event) {
        NamespaceDO namespaceDO = (NamespaceDO) event.getSource();
        if (Objects.isNull(namespaceDO)) {
            return;
        }
        create(namespaceDO.getNamespaceId(), event.getUserId());
    }
}
