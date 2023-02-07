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
    
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.model.entity.MetaDataDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.service.impl.MetaDataServiceImpl;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import com.google.gson.JsonParser;
    
import java.util.ArrayList;
import java.util.List;
    
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
    
/**
 * Test cases for {@link ShenyuClientRegisterSofaServiceImpl}.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class ShenyuClientRegisterSofaServiceImplTest {

    @InjectMocks
    private ShenyuClientRegisterSofaServiceImpl shenyuClientRegisterSofaService;

    @Mock
    private MetaDataServiceImpl metaDataService;

    @Test
    public void testRpcType() {
        String rpcType = shenyuClientRegisterSofaService.rpcType();
        assertEquals(RpcTypeEnum.SOFA.getName(), rpcType);
    }
    
    @Test
    public void testSelectorHandler() {
        MetaDataRegisterDTO metaDataRegisterDTO = MetaDataRegisterDTO.builder().build();
        assertEquals(StringUtils.EMPTY, shenyuClientRegisterSofaService.selectorHandler(metaDataRegisterDTO));
    }
    
    @Test
    public void testRuleHandler() {
        assertEquals(JsonParser.parseString("{\"retries\":0,\"loadBalance\":\"random\",\"timeout\":3000}"), JsonParser.parseString(shenyuClientRegisterSofaService.ruleHandler()));
    }
    
    @Test
    public void testRegisterMetadata() {
        MetaDataDO metaDataDO = MetaDataDO.builder().build();
        when(metaDataService.findByServiceNameAndMethodName(any(), any())).thenReturn(metaDataDO);
        MetaDataRegisterDTO metaDataDTO = MetaDataRegisterDTO.builder().build();
        shenyuClientRegisterSofaService.registerMetadata(metaDataDTO);
        verify(metaDataService).saveOrUpdateMetaData(metaDataDO, metaDataDTO);
    }
    
    @Test
    public void testBuildHandle() {
        List<URIRegisterDTO> list = new ArrayList<>();
        list.add(URIRegisterDTO.builder().build());
        assertEquals(StringUtils.EMPTY,
                shenyuClientRegisterSofaService.buildHandle(list, SelectorDO.builder().build()));
    }
}
