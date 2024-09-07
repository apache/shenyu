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

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import org.apache.shenyu.admin.model.entity.AlertReceiverDO;
import org.apache.shenyu.alert.model.AlertReceiverDTO;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.sql.Timestamp;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * test cast for {@link AlertTransfer}.
 */
public final class AlertTransferTest {

    private AlertReceiverDTO alertReceiverDTO;

    private AlertReceiverDO alertReceiverDO;

    @BeforeEach
    public void setUp() {
        alertReceiverDTO = new AlertReceiverDTO();
        alertReceiverDTO.setId("123");
        alertReceiverDTO.setName("123");
        alertReceiverDTO.setAccessToken("123");
        alertReceiverDTO.setAgentId("123");
        alertReceiverDTO.setCorpId("123");
        alertReceiverDTO.setAppSecret("123");
        alertReceiverDTO.setDiscordBotToken("123");
        alertReceiverDTO.setDiscordChannelId("123");
        alertReceiverDTO.setEmail("123");
        alertReceiverDTO.setWechatId("123");
        alertReceiverDTO.setEnable(true);
        alertReceiverDTO.setHookUrl("123");
        alertReceiverDTO.setType(Byte.valueOf("10"));
        alertReceiverDTO.setLabels(Maps.newHashMap());
        alertReceiverDTO.setLevels(Lists.newArrayList());
        alertReceiverDTO.setTgUserId("123");
        alertReceiverDTO.setTgBotToken("123");
        alertReceiverDTO.setSmnSk("123");
        alertReceiverDTO.setSmnAk("123");
        alertReceiverDTO.setSmnProjectId("123");
        alertReceiverDTO.setSmnTopicUrn("123");
        alertReceiverDTO.setSmnRegion("123");
        alertReceiverDTO.setPhone("123");
        alertReceiverDTO.setMatchAll(true);
        alertReceiverDTO.setSlackWebHookUrl("123");
        Timestamp currentTime = new Timestamp(System.currentTimeMillis());
        alertReceiverDTO.setDateCreated(currentTime);
        alertReceiverDTO.setDateUpdated(currentTime);

        alertReceiverDO = new AlertReceiverDO();
        alertReceiverDO.setId("123");
        alertReceiverDO.setName("123");
        alertReceiverDO.setAccessToken("123");
        alertReceiverDO.setAgentId("123");
        alertReceiverDO.setCorpId("123");
        alertReceiverDO.setAppSecret("123");
        alertReceiverDO.setDiscordBotToken("123");
        alertReceiverDO.setDiscordChannelId("123");
        alertReceiverDO.setEmail("123");
        alertReceiverDO.setWechatId("123");
        alertReceiverDO.setEnable(true);
        alertReceiverDO.setHookUrl("123");
        alertReceiverDO.setType(Byte.valueOf("10"));
        alertReceiverDO.setLabels(Maps.newHashMap());
        alertReceiverDO.setLevels(Lists.newArrayList());
        alertReceiverDO.setTgUserId("123");
        alertReceiverDO.setTgBotToken("123");
        alertReceiverDO.setSmnSk("123");
        alertReceiverDO.setSmnAk("123");
        alertReceiverDO.setSmnProjectId("123");
        alertReceiverDO.setSmnTopicUrn("123");
        alertReceiverDO.setSmnRegion("123");
        alertReceiverDO.setPhone("123");
        alertReceiverDO.setMatchAll(true);
        alertReceiverDO.setSlackWebHookUrl("123");
        alertReceiverDO.setDateCreated(currentTime);
        alertReceiverDO.setDateUpdated(currentTime);
    }

    @Test
    void testMapToEntity() {
        AlertReceiverDO entity = AlertTransfer.INSTANCE.mapToAlertReceiverDO(alertReceiverDTO);
        assertNotNull(entity);
        assertEquals(entity.getId(), alertReceiverDTO.getId());
        assertEquals(entity.getName(), alertReceiverDTO.getName());
        assertEquals(entity.getAccessToken(), alertReceiverDTO.getAccessToken());
        assertEquals(entity.getAgentId(), alertReceiverDTO.getAgentId());
        assertEquals(entity.getCorpId(), alertReceiverDTO.getCorpId());
        assertEquals(entity.getAppSecret(), alertReceiverDTO.getAppSecret());
        assertEquals(entity.getDiscordBotToken(), alertReceiverDTO.getDiscordBotToken());
        assertEquals(entity.getDiscordChannelId(), alertReceiverDTO.getDiscordChannelId());
        assertEquals(entity.getEmail(), alertReceiverDTO.getEmail());
        assertEquals(entity.getWechatId(), alertReceiverDTO.getWechatId());
        assertEquals(entity.isEnable(), alertReceiverDTO.isEnable());
        assertEquals(entity.getHookUrl(), alertReceiverDTO.getHookUrl());
        assertEquals(entity.getType(), alertReceiverDTO.getType());
        assertEquals(entity.getLabels(), alertReceiverDTO.getLabels());
        assertEquals(entity.getLevels(), alertReceiverDTO.getLevels());
        assertEquals(entity.getTgUserId(), alertReceiverDTO.getTgUserId());
        assertEquals(entity.getTgBotToken(), alertReceiverDTO.getTgBotToken());
        assertEquals(entity.getSmnSk(), alertReceiverDTO.getSmnSk());
        assertEquals(entity.getSmnAk(), alertReceiverDTO.getSmnAk());
        assertEquals(entity.getSmnProjectId(), alertReceiverDTO.getSmnProjectId());
        assertEquals(entity.getSmnTopicUrn(), alertReceiverDTO.getSmnTopicUrn());
        assertEquals(entity.getSmnRegion(), alertReceiverDTO.getSmnRegion());
        assertEquals(entity.getPhone(), alertReceiverDTO.getPhone());
        assertEquals(entity.isMatchAll(), alertReceiverDTO.isMatchAll());
        assertEquals(entity.getSlackWebHookUrl(), alertReceiverDTO.getSlackWebHookUrl());
        assertEquals(entity.getDateCreated(), alertReceiverDTO.getDateCreated());
        assertEquals(entity.getDateUpdated(), alertReceiverDTO.getDateUpdated());
    }

    @Test
    void testMapToDTO() {
        AlertReceiverDTO dto = AlertTransfer.INSTANCE.mapToAlertReceiverDTO(alertReceiverDO);
        assertNotNull(dto);
        assertEquals(dto.getId(), alertReceiverDO.getId());
        assertEquals(dto.getName(), alertReceiverDO.getName());
        assertEquals(dto.getAccessToken(), alertReceiverDO.getAccessToken());
        assertEquals(dto.getAgentId(), alertReceiverDO.getAgentId());
        assertEquals(dto.getCorpId(), alertReceiverDO.getCorpId());
        assertEquals(dto.getAppSecret(), alertReceiverDO.getAppSecret());
        assertEquals(dto.getDiscordBotToken(), alertReceiverDO.getDiscordBotToken());
        assertEquals(dto.getDiscordChannelId(), alertReceiverDO.getDiscordChannelId());
        assertEquals(dto.getEmail(), alertReceiverDO.getEmail());
        assertEquals(dto.getWechatId(), alertReceiverDO.getWechatId());
        assertEquals(dto.isEnable(), alertReceiverDO.isEnable());
        assertEquals(dto.getHookUrl(), alertReceiverDO.getHookUrl());
        assertEquals(dto.getType(), alertReceiverDO.getType());
        assertEquals(dto.getLabels(), alertReceiverDO.getLabels());
        assertEquals(dto.getLevels(), alertReceiverDO.getLevels());
        assertEquals(dto.getTgUserId(), alertReceiverDO.getTgUserId());
        assertEquals(dto.getTgBotToken(), alertReceiverDO.getTgBotToken());
        assertEquals(dto.getSmnSk(), alertReceiverDO.getSmnSk());
        assertEquals(dto.getSmnAk(), alertReceiverDO.getSmnAk());
        assertEquals(dto.getSmnProjectId(), alertReceiverDO.getSmnProjectId());
        assertEquals(dto.getSmnTopicUrn(), alertReceiverDO.getSmnTopicUrn());
        assertEquals(dto.getSmnRegion(), alertReceiverDO.getSmnRegion());
        assertEquals(dto.getPhone(), alertReceiverDO.getPhone());
        assertEquals(dto.isMatchAll(), alertReceiverDO.isMatchAll());
        assertEquals(dto.getSlackWebHookUrl(), alertReceiverDO.getSlackWebHookUrl());
        assertEquals(dto.getDateCreated(), alertReceiverDO.getDateCreated());
        assertEquals(dto.getDateUpdated(), alertReceiverDO.getDateUpdated());
    }
}
