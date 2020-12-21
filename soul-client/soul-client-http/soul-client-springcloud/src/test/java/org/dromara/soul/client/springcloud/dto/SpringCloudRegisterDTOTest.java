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

package org.dromara.soul.client.springcloud.dto;

import org.apache.commons.lang3.RandomStringUtils;
import org.hamcrest.Matchers;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

/**
 * Test Case for SpringCloudRegisterDTO.
 *
 * @author DaveModl (davemo-coderpersonal@hotmail.com)
 */
public final class SpringCloudRegisterDTOTest {

    private static final String APP_NAME = RandomStringUtils.random(5);

    private static final String CONTEXT = RandomStringUtils.random(5);

    private static final String PATH = RandomStringUtils.random(5);

    private static final String PATH_DESC = RandomStringUtils.random(10);

    private static final String RPC_TYPE = "springCloud";

    private static final String RULE_NAME = RandomStringUtils.random(5);

    private static final Boolean ENABLED = true;

    private SpringCloudRegisterDTO springCloudRegisterDTO;

    private SpringCloudRegisterDTO other;

    @Before
    public void build() {
        springCloudRegisterDTO = SpringCloudRegisterDTO.builder()
                .appName(APP_NAME)
                .context(CONTEXT)
                .path(PATH)
                .pathDesc(PATH_DESC)
                .rpcType(RPC_TYPE)
                .ruleName(RULE_NAME)
                .enabled(ENABLED)
                .build();

        other = new SpringCloudRegisterDTO(APP_NAME, CONTEXT, PATH, PATH_DESC, RPC_TYPE, RULE_NAME, ENABLED);
    }

    @Test
    public void testSpringCloudRegisterDTOCreate() {
        Assert.assertNotNull(springCloudRegisterDTO);
        Assert.assertThat(springCloudRegisterDTO.getAppName(), Matchers.equalTo(APP_NAME));
        Assert.assertThat(springCloudRegisterDTO.getContext(), Matchers.equalTo(CONTEXT));
        Assert.assertThat(springCloudRegisterDTO.getPath(), Matchers.equalTo(PATH));
        Assert.assertThat(springCloudRegisterDTO.getPathDesc(), Matchers.equalTo(PATH_DESC));
        Assert.assertThat(springCloudRegisterDTO.getRpcType(), Matchers.equalTo(RPC_TYPE));
        Assert.assertThat(springCloudRegisterDTO.getRuleName(), Matchers.equalTo(RULE_NAME));
        Assert.assertThat(springCloudRegisterDTO.isEnabled(), Matchers.equalTo(ENABLED));
        Assert.assertEquals(springCloudRegisterDTO, other);
    }
}
