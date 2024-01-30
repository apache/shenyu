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

package org.apache.shenyu.admin.model.event.user;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.model.entity.DashboardUserDO;
import org.apache.shenyu.admin.model.enums.EventTypeEnum;
import org.apache.shenyu.common.utils.DigestUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * test case for {@link UserUpdatedEvent}.
 */
public class UserUpdatedEventTest {

    private DashboardUserDO before;

    private DashboardUserDO after;

    @BeforeEach
    public void setUp() {
        before = DashboardUserDO.builder()
                .id("1")
                .userName("adminBeforeTest")
                .password(DigestUtils.sha512Hex("123456"))
                .role(1)
                .enabled(true)
                .build();

        after = DashboardUserDO.builder()
                .id("1")
                .userName("adminAfterTest")
                .password(DigestUtils.sha512Hex("123457"))
                .role(2)
                .enabled(false)
                .build();
    }

    @Test
    public void userUpdatedContextTest() {
        String typeStr = StringUtils.lowerCase(EventTypeEnum.USER_UPDATE.getType().toString());

        UserUpdatedEvent userUpdatedNothingEvent = new UserUpdatedEvent(after, after, "test-operator");
        String contextNothing = String.format("the selector [%s] is %s : %s",
                after.getUserName(), typeStr, "it no change");
        assertEquals(contextNothing, userUpdatedNothingEvent.buildContext());

        StringBuilder contrast = new StringBuilder();
        contrast.append(String.format("name[%s => %s] ", before.getUserName(), after.getUserName()));
        contrast.append("password is changed...");
        contrast.append(String.format("role[%s => %s] ", before.getRole(), after.getRole()));
        contrast.append(String.format("enable[%s => %s] ", before.getEnabled(), after.getEnabled()));

        String context = String.format("the selector [%s] is %s : %s",
                after.getUserName(), typeStr, contrast);
        UserUpdatedEvent userUpdatedEvent = new UserUpdatedEvent(after, before, "test-operator");
        assertEquals(context, userUpdatedEvent.buildContext());
    }
}
