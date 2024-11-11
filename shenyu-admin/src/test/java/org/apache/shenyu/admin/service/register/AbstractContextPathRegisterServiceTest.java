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

import org.apache.shenyu.admin.mapper.NamespacePluginRelMapper;
import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.model.vo.NamespacePluginVO;
import org.apache.shenyu.admin.service.impl.RuleServiceImpl;
import org.apache.shenyu.admin.service.impl.SelectorServiceImpl;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.jetbrains.annotations.NotNull;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.integration.support.locks.LockRegistry;

import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.verify;


/**
 * Test cases for AbstractContextPathRegisterService.
 */
@ExtendWith(MockitoExtension.class)
class AbstractContextPathRegisterServiceTest {

    @InjectMocks
    private MockAbstractContextPathRegisterService abstractContextPathRegisterService = new MockAbstractContextPathRegisterService();

    @Mock
    private SelectorServiceImpl selectorService;

    @Mock
    private RuleServiceImpl ruleService;

    @Mock
    private LockRegistry registry;

    @Mock
    private NamespacePluginRelMapper namespacePluginRelMapper;

    @Mock
    private PluginMapper pluginMapper;

    @Test
    public void testRegisterContextPath() {
        MetaDataRegisterDTO dto = MetaDataRegisterDTO.builder().build();
        dto.setContextPath("Context_Path");
        dto.setAddPrefixed(true);
        dto.setNamespaceId(Constants.SYS_DEFAULT_NAMESPACE_ID);
        when(selectorService.registerDefault(dto, PluginEnum.CONTEXT_PATH.getName(), "")).thenReturn("Context_Path_Selector_Id");
        // org.springframework.integration.jdbc.lock.JdbcLockRegistry.JdbcLock is private and cannot be mocked directly so we mock the LockRegistry and return a mock MockLock
        // here  mock ReentrantLock   cause cpu usage 100% in the jdk 19 20 21 environment
        when(registry.obtain(any())).thenReturn(mock(MockLock.class));
        when(ruleService.findBySelectorIdAndName("Context_Path_Selector_Id", "Context_Path")).thenReturn(null);
        when(ruleService.registerDefault(any())).thenReturn("Context_Path_Rule_Id");
        PluginDO pluginDO = new PluginDO();
        pluginDO.setId("1");
        pluginDO.setName(PluginEnum.CONTEXT_PATH.getName());
        when(pluginMapper.selectByName(PluginEnum.CONTEXT_PATH.getName())).thenReturn(pluginDO);
        NamespacePluginVO namespacePluginVO = new NamespacePluginVO();
        namespacePluginVO.setPluginId("1");
        when(namespacePluginRelMapper.selectByPluginIdAndNamespaceId(pluginDO.getId(), Constants.SYS_DEFAULT_NAMESPACE_ID)).thenReturn(namespacePluginVO);
        abstractContextPathRegisterService.registerContextPath(dto);
        verify(ruleService).registerDefault(any());
    }

    static class MockLock implements Lock {
        @Override
        public void lock() {

        }

        @Override
        public void lockInterruptibly() throws InterruptedException {

        }

        @Override
        public boolean tryLock() {
            return false;
        }

        @Override
        public boolean tryLock(final long time, @NotNull final TimeUnit unit) throws InterruptedException {
            return false;
        }

        @Override
        public void unlock() {

        }

        @NotNull
        @Override
        public Condition newCondition() {
            return null;
        }
    }

    static class MockAbstractContextPathRegisterService extends AbstractContextPathRegisterService {

        @Override
        protected String selectorHandler(final MetaDataRegisterDTO metaDataDTO) {
            return null;
        }

        @Override
        protected String ruleHandler() {
            return null;
        }

        @Override
        protected void registerMetadata(final MetaDataRegisterDTO metaDataDTO) {

        }

        @Override
        protected String buildHandle(final List<URIRegisterDTO> uriList, final SelectorDO selectorDO) {
            return null;
        }

        @Override
        public String rpcType() {
            return null;
        }
    }
}
