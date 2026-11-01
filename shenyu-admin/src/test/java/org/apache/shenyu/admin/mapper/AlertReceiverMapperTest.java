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

package org.apache.shenyu.admin.mapper;

import org.apache.shenyu.admin.AbstractSpringIntegrationTest;
import org.apache.shenyu.admin.model.entity.AlertReceiverDO;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.junit.jupiter.api.Test;

import jakarta.annotation.Resource;
import java.sql.Timestamp;
import java.util.Collections;

import static org.apache.shenyu.common.constant.Constants.SYS_DEFAULT_NAMESPACE_ID;
import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Test cases for AlertReceiverMapper.
 */
public final class AlertReceiverMapperTest extends AbstractSpringIntegrationTest {

    @Resource
    private AlertReceiverMapper alertReceiverMapper;

    @Test
    public void testUpdateAccessTokenSelective() {
        AlertReceiverDO receiver = new AlertReceiverDO();
        receiver.setId(UUIDUtils.getInstance().generateShortUuid());
        receiver.setName("DingTalk receiver");
        receiver.setEnable(true);
        receiver.setType((byte) 5);
        receiver.setMatchAll(false);
        receiver.setNamespaceId(SYS_DEFAULT_NAMESPACE_ID);
        Timestamp now = new Timestamp(System.currentTimeMillis());
        receiver.setDateCreated(now);
        receiver.setDateUpdated(now);
        receiver.setAccessToken("original-token");

        try {
            assertEquals(1, alertReceiverMapper.insert(receiver));

            AlertReceiverDO update = new AlertReceiverDO();
            update.setId(receiver.getId());
            update.setAccessToken("dingtalk-token-abc123");
            assertEquals(1, alertReceiverMapper.updateByPrimaryKeySelective(update));

            AlertReceiverDO updated = alertReceiverMapper.selectByPrimaryKey(receiver.getId());
            assertEquals("dingtalk-token-abc123", updated.getAccessToken());
            assertEquals("DingTalk receiver", updated.getName());
        } finally {
            alertReceiverMapper.deleteByIds(Collections.singletonList(receiver.getId()));
        }
    }
}
