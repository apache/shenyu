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

import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.service.impl.RuleServiceImpl;
import org.apache.shenyu.admin.service.impl.SelectorServiceImpl;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.List;

import static org.mockito.ArgumentMatchers.any;
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

    @Test
    public void testRegisterContextPath() {
        MetaDataRegisterDTO dto = MetaDataRegisterDTO.builder().build();
        dto.setContextPath("Context_Path");
        dto.setAddPrefixed(true);
        when(selectorService.registerDefault(dto, PluginEnum.CONTEXT_PATH.getName(), "")).thenReturn("Context_Path_Selector_Id");
        abstractContextPathRegisterService.registerContextPath(dto);
        verify(ruleService).registerDefault(any());
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
