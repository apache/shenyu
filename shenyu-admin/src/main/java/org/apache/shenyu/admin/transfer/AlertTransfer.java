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

import org.apache.shenyu.admin.model.entity.AlertReceiverDO;
import org.apache.shenyu.alert.model.AlertReceiverDTO;

import java.util.Optional;

/**
 * The Alert transfer.
 */
public enum AlertTransfer {

    /**
     * The constant INSTANCE.
     */
    INSTANCE;

    /**
     * Map to entity alert receiver do.
     *
     * @param alertReceiverDTO the alert receiver dto
     * @return the alert receiver do
     */
    public AlertReceiverDO mapToAlertReceiverDO(final AlertReceiverDTO alertReceiverDTO) {
        return Optional.ofNullable(alertReceiverDTO)
                .map(v -> {
                    AlertReceiverDO alertReceiverDO = new AlertReceiverDO();
                    alertReceiverDO.setId(alertReceiverDTO.getId());
                    alertReceiverDO.setName(alertReceiverDTO.getName());
                    alertReceiverDO.setAccessToken(alertReceiverDTO.getAccessToken());
                    alertReceiverDO.setAgentId(alertReceiverDTO.getAgentId());
                    alertReceiverDO.setCorpId(alertReceiverDTO.getCorpId());
                    alertReceiverDO.setAppSecret(alertReceiverDTO.getAppSecret());
                    alertReceiverDO.setDiscordBotToken(alertReceiverDTO.getDiscordBotToken());
                    alertReceiverDO.setDiscordChannelId(alertReceiverDTO.getDiscordChannelId());
                    alertReceiverDO.setEmail(alertReceiverDTO.getEmail());
                    alertReceiverDO.setWechatId(alertReceiverDTO.getWechatId());
                    alertReceiverDO.setEnable(alertReceiverDTO.isEnable());
                    alertReceiverDO.setHookUrl(alertReceiverDTO.getHookUrl());
                    alertReceiverDO.setType(alertReceiverDTO.getType());
                    alertReceiverDO.setLabels(alertReceiverDTO.getLabels());
                    alertReceiverDO.setLevels(alertReceiverDTO.getLevels());
                    alertReceiverDO.setTgUserId(alertReceiverDTO.getTgUserId());
                    alertReceiverDO.setTgBotToken(alertReceiverDTO.getTgBotToken());
                    alertReceiverDO.setSmnSk(alertReceiverDTO.getSmnSk());
                    alertReceiverDO.setSmnAk(alertReceiverDTO.getSmnAk());
                    alertReceiverDO.setSmnProjectId(alertReceiverDTO.getSmnProjectId());
                    alertReceiverDO.setSmnTopicUrn(alertReceiverDTO.getSmnTopicUrn());
                    alertReceiverDO.setSmnRegion(alertReceiverDTO.getSmnRegion());
                    alertReceiverDO.setPhone(alertReceiverDTO.getPhone());
                    alertReceiverDO.setMatchAll(alertReceiverDTO.isMatchAll());
                    alertReceiverDO.setSlackWebHookUrl(alertReceiverDTO.getSlackWebHookUrl());
                    alertReceiverDO.setNamespaceId(alertReceiverDTO.getNamespaceId());
                    alertReceiverDO.setDateCreated(alertReceiverDTO.getDateCreated());
                    alertReceiverDO.setDateUpdated(alertReceiverDTO.getDateUpdated());
                    return alertReceiverDO;
                })
                .orElse(null);
    }

    /**
     * Map to alert receiver dto.
     *
     * @param alertReceiverDO the alert receiver do
     * @return the alert receiver do
     */
    public AlertReceiverDTO mapToAlertReceiverDTO(final AlertReceiverDO alertReceiverDO) {
        return Optional.ofNullable(alertReceiverDO)
                .map(v -> {
                    AlertReceiverDTO alertReceiverDTO = new AlertReceiverDTO();
                    alertReceiverDTO.setId(alertReceiverDO.getId());
                    alertReceiverDTO.setName(alertReceiverDO.getName());
                    alertReceiverDTO.setAccessToken(alertReceiverDO.getAccessToken());
                    alertReceiverDTO.setAgentId(alertReceiverDO.getAgentId());
                    alertReceiverDTO.setCorpId(alertReceiverDO.getCorpId());
                    alertReceiverDTO.setAppSecret(alertReceiverDO.getAppSecret());
                    alertReceiverDTO.setDiscordBotToken(alertReceiverDO.getDiscordBotToken());
                    alertReceiverDTO.setDiscordChannelId(alertReceiverDO.getDiscordChannelId());
                    alertReceiverDTO.setEmail(alertReceiverDO.getEmail());
                    alertReceiverDTO.setWechatId(alertReceiverDO.getWechatId());
                    alertReceiverDTO.setEnable(alertReceiverDO.isEnable());
                    alertReceiverDTO.setHookUrl(alertReceiverDO.getHookUrl());
                    alertReceiverDTO.setType(alertReceiverDO.getType());
                    alertReceiverDTO.setLabels(alertReceiverDO.getLabels());
                    alertReceiverDTO.setLevels(alertReceiverDO.getLevels());
                    alertReceiverDTO.setTgUserId(alertReceiverDO.getTgUserId());
                    alertReceiverDTO.setTgBotToken(alertReceiverDO.getTgBotToken());
                    alertReceiverDTO.setSmnSk(alertReceiverDO.getSmnSk());
                    alertReceiverDTO.setSmnAk(alertReceiverDO.getSmnAk());
                    alertReceiverDTO.setSmnProjectId(alertReceiverDO.getSmnProjectId());
                    alertReceiverDTO.setSmnTopicUrn(alertReceiverDO.getSmnTopicUrn());
                    alertReceiverDTO.setSmnRegion(alertReceiverDO.getSmnRegion());
                    alertReceiverDTO.setPhone(alertReceiverDO.getPhone());
                    alertReceiverDTO.setMatchAll(alertReceiverDO.isMatchAll());
                    alertReceiverDTO.setSlackWebHookUrl(alertReceiverDO.getSlackWebHookUrl());
                    alertReceiverDTO.setDateCreated(alertReceiverDO.getDateCreated());
                    alertReceiverDTO.setDateUpdated(alertReceiverDO.getDateUpdated());
                    return alertReceiverDTO;
                })
                .orElse(null);
    }


}
