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

package org.apache.shenyu.admin.transfer;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.shenyu.admin.model.entity.RegistryDO;
import org.apache.shenyu.admin.model.vo.RegistryVO;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * Test for {@link RegistryTransfer}.
 */
public class RegistryTransferTest {

    @Test
    public void mapToVoShouldNotExposePasswordInSerializedJson() throws Exception {
        RegistryDO registryDO = RegistryDO.builder()
                .id("registry-id")
                .registryId("registry")
                .protocol("http")
                .address("127.0.0.1:2181")
                .username("admin")
                .password("secret-password")
                .namespace("default")
                .registryGroup("group")
                .build();

        RegistryVO registryVO = RegistryTransfer.INSTANCE.mapToVo(registryDO);
        assertNotNull(registryVO);

        String json = new ObjectMapper().writeValueAsString(registryVO);
        assertFalse(json.contains("secret-password"));
        assertFalse(json.contains("\"password\""));
    }
}
