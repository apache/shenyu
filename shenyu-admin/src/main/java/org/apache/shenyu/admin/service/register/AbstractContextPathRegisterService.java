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

package org.apache.shenyu.admin.service.register;

import org.apache.shenyu.admin.lock.RegisterExecutionLock;
import org.apache.shenyu.admin.lock.RegisterExecutionRepository;
import org.apache.shenyu.common.utils.PathUtils;
import org.apache.shenyu.common.dto.convert.rule.impl.ContextMappingRuleHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import javax.annotation.Resource;
import java.util.Objects;

/**
 * The type Abstract context path register service.
 */
public abstract class AbstractContextPathRegisterService extends AbstractShenyuClientRegisterServiceImpl {

    @Resource
    private RegisterExecutionRepository registerExecutionRepository;

    @Override
    public void registerContextPath(final MetaDataRegisterDTO dto) {
        String name = PluginEnum.CONTEXT_PATH.getName();
        String contextPathSelectorId = getSelectorService().registerDefault(dto, name, "");
        // avoid repeated registration for many client threads
        // many client threads may register the same context path for contextPath plugin at the same time
        RegisterExecutionLock lock = registerExecutionRepository.getLock(name);
        lock.lock();
        try {
            if (Objects.nonNull(getRuleService().findBySelectorIdAndName(contextPathSelectorId, dto.getContextPath()))) {
                return;
            }
            ContextMappingRuleHandle handle = new ContextMappingRuleHandle();
            handle.setContextPath(PathUtils.decoratorContextPath(dto.getContextPath()));
            handle.setAddPrefixed(dto.getAddPrefixed());
            getRuleService().registerDefault(buildContextPathDefaultRuleDTO(contextPathSelectorId, dto, handle.toJson()));
        } finally {
            lock.unlock();
        }
    }
}
