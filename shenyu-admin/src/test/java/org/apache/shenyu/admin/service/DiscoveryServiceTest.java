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

package org.apache.shenyu.admin.service;

import org.apache.shenyu.admin.discovery.DefaultDiscoveryProcessor;
import org.apache.shenyu.admin.discovery.DiscoveryProcessor;
import org.apache.shenyu.admin.discovery.DiscoveryProcessorHolder;
import org.apache.shenyu.admin.discovery.LocalDiscoveryProcessor;
import org.apache.shenyu.admin.mapper.DiscoveryHandlerMapper;
import org.apache.shenyu.admin.mapper.DiscoveryMapper;
import org.apache.shenyu.admin.mapper.DiscoveryRelMapper;
import org.apache.shenyu.admin.mapper.DiscoveryUpstreamMapper;
import org.apache.shenyu.admin.mapper.ProxySelectorMapper;
import org.apache.shenyu.admin.model.dto.DiscoveryDTO;
import org.apache.shenyu.admin.model.entity.DiscoveryDO;
import org.apache.shenyu.admin.model.entity.DiscoveryHandlerDO;
import org.apache.shenyu.admin.model.entity.DiscoveryUpstreamDO;
import org.apache.shenyu.admin.model.entity.ProxySelectorDO;
import org.apache.shenyu.admin.model.vo.DiscoveryVO;
import org.apache.shenyu.admin.service.impl.DiscoveryServiceImpl;
import org.apache.shenyu.admin.service.impl.DiscoveryUpstreamServiceImpl;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.register.common.dto.DiscoveryConfigRegisterDTO;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Spy;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.util.ArrayList;
import java.util.List;
import java.util.WeakHashMap;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class DiscoveryServiceTest {

    @InjectMocks
    private DiscoveryServiceImpl discoveryService;

    @Mock
    private DiscoveryMapper discoveryMapper;

    @Mock
    private ProxySelectorMapper proxySelectorMapper;

    @Mock
    private DiscoveryHandlerMapper discoveryHandlerMapper;

    @Mock
    private DiscoveryRelMapper discoveryRelMapper;

    @Mock
    private DiscoveryProcessorHolder discoveryProcessorHolder;

    @Mock
    private DefaultDiscoveryProcessor defaultDiscoveryProcessor;

    @BeforeEach
    void setUp() {
        discoveryService = new DiscoveryServiceImpl(discoveryMapper, proxySelectorMapper, discoveryRelMapper, discoveryHandlerMapper,
                discoveryProcessorHolder);
    }

    @Test
    void testTypeEnums(){
        List<String> list = discoveryService.typeEnums();
        assertThat(list.size(), greaterThan(0));
    }

    @Test
    void testDiscovery(){
        DiscoveryDO discoveryDO=new DiscoveryDO("1","datecreated","level","serverlist","name","prop");
        when(discoveryMapper.selectByPluginNameAndLevel(anyString(), anyString())).thenReturn(discoveryDO);
        DiscoveryVO discovery = discoveryService.discovery("pluginName", "level");
        assertNotNull(discovery);
    }

    @Test
    void testCreateOrUpdate(){
        DiscoveryDTO discoveryDTO=new DiscoveryDTO();
        discoveryDTO.setType("zookeeper");
        when(discoveryProcessorHolder.chooseProcessor(anyString())).thenReturn(defaultDiscoveryProcessor);
        when(discoveryMapper.insert(any(DiscoveryDO.class))).thenReturn(1);
        doNothing().when(defaultDiscoveryProcessor).createDiscovery(any(DiscoveryDO.class));
        DiscoveryVO orUpdate = discoveryService.createOrUpdate(discoveryDTO);
        assertNotNull(orUpdate);

        DiscoveryDTO discoveryDTO2=new DiscoveryDTO();
        discoveryDTO2.setType("zookeeper");
        discoveryDTO2.setId("11");
        when(discoveryMapper.updateSelective(any(DiscoveryDO.class))).thenReturn(1);
        DiscoveryVO orUpdate2 = discoveryService.createOrUpdate(discoveryDTO2);
        assertNotNull(orUpdate2);
    }

    @Test
    void testRegisterDiscoveryConfig(){
        DiscoveryConfigRegisterDTO discoveryConfigRegisterDTO=new DiscoveryConfigRegisterDTO();
        discoveryConfigRegisterDTO.setDiscoveryType("zookeeper");
        discoveryConfigRegisterDTO.setName("zk discovery");
        discoveryConfigRegisterDTO.setPluginName("divide");
        discoveryConfigRegisterDTO.setServerList("127.0.0.1:2181");

        when(discoveryMapper.selectByName(anyString())).thenReturn(new DiscoveryDO());
        discoveryService.registerDiscoveryConfig(discoveryConfigRegisterDTO);
        verify(discoveryMapper).selectByName(anyString());
    }

    @Test
    void testDelete(){
        List<DiscoveryHandlerDO> discoveryHandlerDOS =new ArrayList<>();
        DiscoveryHandlerDO discoveryHandlerDO=new DiscoveryHandlerDO();
        discoveryHandlerDOS.add(discoveryHandlerDO);
        when(discoveryHandlerMapper.selectByDiscoveryId(anyString())).thenReturn(discoveryHandlerDOS);
        assertThrows(ShenyuException.class,()->{
            discoveryService.delete("1");
        });
        DiscoveryDO discoveryDO =new DiscoveryDO();
        discoveryDO.setType("zk");
        when(discoveryMapper.selectById(anyString())).thenReturn(discoveryDO);
        when(discoveryProcessorHolder.chooseProcessor(anyString())).thenReturn(defaultDiscoveryProcessor);
        when(discoveryHandlerMapper.selectByDiscoveryId(anyString())).thenReturn(null);
        doNothing().when(defaultDiscoveryProcessor).removeDiscovery(any(DiscoveryDO.class));
        assertEquals(ShenyuResultMessage.DELETE_SUCCESS, discoveryService.delete("1"));

    }

}
