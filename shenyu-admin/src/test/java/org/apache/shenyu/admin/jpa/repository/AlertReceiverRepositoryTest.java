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

package org.apache.shenyu.admin.jpa.repository;

import jakarta.annotation.Resource;
import org.apache.shenyu.admin.AbstractSpringIntegrationTest;
import org.apache.shenyu.admin.model.entity.AlertReceiverDO;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.page.PageResultUtils;
import org.apache.shenyu.admin.model.query.AlertReceiverQuery;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.junit.jupiter.api.Test;
import org.springframework.data.domain.Page;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;

class AlertReceiverRepositoryTest extends AbstractSpringIntegrationTest {

    @Resource
    private AlertReceiverRepository alertReceiverRepository;

    @Test
    void pageByDynamicConditions() {
        int namespaceACount = 6;
        for (int i = 0; i < namespaceACount; i++) {
            alertReceiverRepository.save(buildAlertReceiverDO("namespaceA"));
        }
        alertReceiverRepository.save(buildAlertReceiverDO("namespaceB"));

        int pageSize = 5;
        AlertReceiverQuery query = new AlertReceiverQuery();
        query.setNamespaceId("namespaceA");
        query.setPageParameter(new PageParameter(1, pageSize));
        Page<AlertReceiverDO> firstPage = alertReceiverRepository.pageByDynamicConditions(query, PageResultUtils.of(query.getPageParameter()));
        assertEquals(namespaceACount, firstPage.getTotalElements());
        assertEquals(pageSize, firstPage.getContent().size());

        query.setPageParameter(new PageParameter(2, pageSize));
        Page<AlertReceiverDO> secondPage = alertReceiverRepository.pageByDynamicConditions(query, PageResultUtils.of(query.getPageParameter()));
        assertEquals(namespaceACount, secondPage.getTotalElements());
        assertEquals(namespaceACount - pageSize, secondPage.getContent().size());
    }

    private AlertReceiverDO buildAlertReceiverDO(final String namespaceId) {
        AlertReceiverDO alertReceiverDO = new AlertReceiverDO();
        alertReceiverDO.setId(UUIDUtils.getInstance().generateShortUuid());
        alertReceiverDO.setNamespaceId(namespaceId);
        alertReceiverDO.setName("Default Receiver");
        alertReceiverDO.setEnable(true);
        alertReceiverDO.setType((byte) 1);
        alertReceiverDO.setPhone("1234567890");
        alertReceiverDO.setEmail("default@example.com");
        alertReceiverDO.setWechatId("wechat123");
        alertReceiverDO.setHookUrl("https://webhook.example.com");
        alertReceiverDO.setCorpId("corp123");
        alertReceiverDO.setAgentId("agent123");
        alertReceiverDO.setAppSecret("secret123");
        alertReceiverDO.setAccessToken("access123");
        alertReceiverDO.setTgBotToken("tg_bot_token");
        alertReceiverDO.setTgUserId("tg_user_id");
        alertReceiverDO.setSlackWebHookUrl("https://slack.webhook.url");
        alertReceiverDO.setDiscordChannelId("discord_channel");
        alertReceiverDO.setDiscordBotToken("discord_bot_token");
        alertReceiverDO.setSmnAk("smn_ak");
        alertReceiverDO.setSmnSk("smn_sk");
        alertReceiverDO.setSmnProjectId("smn_project_id");
        alertReceiverDO.setSmnRegion("smn_region");
        alertReceiverDO.setSmnTopicUrn("smn_topic_urn");
        alertReceiverDO.setMatchAll(true);
        Map<String, String> labels = new HashMap<>();
        labels.put("environment", "test");
        labels.put("service", "shenyu");
        alertReceiverDO.setLabels(labels);
        List<Byte> levels = Arrays.asList((byte) 0, (byte) 1, (byte) 2);
        alertReceiverDO.setLevels(levels);
        return alertReceiverDO;
    }
}