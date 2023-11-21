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

import java.util.Arrays;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.model.entity.DashboardUserDO;
import org.apache.shenyu.admin.model.enums.EventTypeEnum;
import org.apache.shenyu.common.utils.DigestUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * test case for {@link BatchUserDeletedEvent}.
 */
public class BatchUserDeletedEventTest {

    private DashboardUserDO one;

    private DashboardUserDO two;

    @BeforeEach
    public void setUp() {
        one = DashboardUserDO.builder()
                .id("1")
                .userName("adminOneTest")
                .password(DigestUtils.sha512Hex("123456"))
                .role(1)
                .enabled(true)
                .build();

        two = DashboardUserDO.builder()
                .id("2")
                .userName("adminTwoTest")
                .password(DigestUtils.sha512Hex("123457"))
                .role(2)
                .enabled(true)
                .build();
    }

    @Test
    public void batchUserDeletedContextTest() {
        BatchUserDeletedEvent batchUserDeletedEvent = new BatchUserDeletedEvent(Arrays.asList(one, two), "test-operator");

        String context = String.format("the user[%s] is %s",
                one.getUserName() + "," + two.getUserName(), StringUtils.lowerCase(EventTypeEnum.USER_DELETE.getType().toString()));

        assertEquals(context, batchUserDeletedEvent.buildContext());
    }
}
