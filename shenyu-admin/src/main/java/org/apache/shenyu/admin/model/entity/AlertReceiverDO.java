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

package org.apache.shenyu.admin.model.entity;

import java.util.Date;
import java.util.List;
import java.util.Map;

/**
 * AlertReceiver.
 */
public class AlertReceiverDO {
    
    /**
     * primary key id.
     */
    private String id;
    
    /**
     * receiver name.
     */
    private String name;
    
    /**
     * is enabled this receiver.
     */
    private boolean enable = true;
    
    /**
     * Notification information method: 0-SMS 1-Email 2-webhook 3-WeChat Official Account. 
     * 4-Enterprise WeChat Robot 5-DingTalk Robot 6-FeiShu Robot. 
     * 7-Telegram Bot 8-SlackWebHook 9-Discord Bot 10-Enterprise WeChat app message.
     */
    private Byte type;
    
    /**
     * Mobile number: Valid when the notification method is SMS.
     */
    private String phone;
    
    /**
     * Email account: Valid when the notification method is email.
     */
    private String email;
    
    /**
     * URL address: The notification method is valid for webhook.
     */
    private String hookUrl;
    
    /**
     * The notification method is valid for WeChat official account, enterprise WeChat robot or FlyBook robot.
     */
    private String wechatId;
    
    /**
     * Access token : The notification method is valid for DingTalk robot.
     */
    private String accessToken;
    
    /**
     * Telegram bot token : The notification method is valid for Telegram Bot.
     */
    private String tgBotToken;
    
    /**
     * Telegram user id: The notification method is valid for Telegram Bot.
     */
    private String tgUserId;
    
    /**
     * URL address: The notification method is valid for Slack.
     */
    private String slackWebHookUrl;
    
    /**
     * Enterprise weChat message: The notification method is valid for Enterprise WeChat app message.
     */
    private String corpId;
    
    /**
     * Enterprise weChat appId: The notification method is valid for Enterprise WeChat app message.
     */
    private String agentId;
    
    /**
     * Enterprise weChat secret: The notification method is valid for Enterprise WeChat app message.
     */
    private String appSecret;
    
    /**
     * Discord channel id: The notification method is valid for Discord.
     */
    private String discordChannelId;
    
    /**
     * Discord bot token: The notification method is valid for Discord.
     */
    private String discordBotToken;
    
    /**
     * huawei cloud SMN ak: If the notification method is valid for huawei cloud SMN.
     */
    private String smnAk;
    
    /**
     * huawei cloud SMN sk: If the notification method is valid for huawei cloud SMN.
     */
    private String smnSk;
    
    /**
     * huawei cloud SMN projectId: If the notification method is valid for huawei cloud SMN.
     */
    private String smnProjectId;
    
    /**
     * huawei cloud SMN region: If the notification method is valid for huawei cloud SMN.
     */
    private String smnRegion;
    
    /**
     * huawei cloud SMN TopicUrn: If the notification method is valid for huawei cloud SMN.
     */
    private String smnTopicUrn;
    
    /**
     * match all.
     */
    private boolean matchAll = true;
    
    /**
     * match alert levels.
     */
    private List<Byte> levels;
    
    /**
     * match alert labels.
     */
    private Map<String, String> labels;
    
    /**
     * create time.
     */
    private Date dateCreated;
    
    /**
     * update time.
     */
    private Date dateUpdated;
    
    /**
     * get id.
     * @return id
     */
    public String getId() {
        return id;
    }
    
    /**
     * set id.
     * @param id id.
     */
    public void setId(final String id) {
        this.id = id;
    }
    
    /**
     * get name.
     * @return name.
     */
    public String getName() {
        return name;
    }
    
    /**
     * set name.
     * @param name name
     */
    public void setName(final String name) {
        this.name = name;
    }
    
    /**
     * is enable.
     * @return enable
     */
    public boolean isEnable() {
        return enable;
    }
    
    /**
     * set enable.
     * @param enable enable
     */
    public void setEnable(final boolean enable) {
        this.enable = enable;
    }
    
    /**
     * get type.
     * @return type
     */
    public Byte getType() {
        return type;
    }
    
    /**
     * set type.
     * @param type type
     */
    public void setType(final Byte type) {
        this.type = type;
    }
    
    /**
     * get phone.
     * @return phone
     */
    public String getPhone() {
        return phone;
    }
    
    /**
     * set phone.
     * @param phone phone
     */
    public void setPhone(final String phone) {
        this.phone = phone;
    }
    
    /**
     * get email.
     * @return email
     */
    public String getEmail() {
        return email;
    }
    
    /**
     * set email.
     * @param email email
     */
    public void setEmail(final String email) {
        this.email = email;
    }
    
    /**
     * get hook url.
     * @return hook url
     */
    public String getHookUrl() {
        return hookUrl;
    }
    
    /**
     * set hook url.
     * @param hookUrl hook url
     */
    public void setHookUrl(final String hookUrl) {
        this.hookUrl = hookUrl;
    }
    
    /**
     * get wechat id.
     * @return wechat id
     */
    public String getWechatId() {
        return wechatId;
    }
    
    /**
     * set wechat id.
     * @param wechatId wechat id
     */
    public void setWechatId(final String wechatId) {
        this.wechatId = wechatId;
    }
    
    /**
     * get access token.
     * @return access token
     */
    public String getAccessToken() {
        return accessToken;
    }
    
    /**
     * set access token.
     * @param accessToken access token
     */
    public void setAccessToken(final String accessToken) {
        this.accessToken = accessToken;
    }
    
    /**
     * get tg bot token.
     * @return tg bot token
     */
    public String getTgBotToken() {
        return tgBotToken;
    }
    
    /**
     * set tg bot token.
     * @param tgBotToken tg bot token
     */
    public void setTgBotToken(final String tgBotToken) {
        this.tgBotToken = tgBotToken;
    }
    
    /**
     * get tg user id.
     * @return tg user id
     */
    public String getTgUserId() {
        return tgUserId;
    }
    
    /**
     * set tg user id.
     * @param tgUserId tg user id
     */
    public void setTgUserId(final String tgUserId) {
        this.tgUserId = tgUserId;
    }
    
    /**
     * get slack web hook url.
     * @return slack web hook url
     */
    public String getSlackWebHookUrl() {
        return slackWebHookUrl;
    }
    
    /**
     * set slack webhook url.
     * @param slackWebHookUrl slack webhook url
     */
    public void setSlackWebHookUrl(final String slackWebHookUrl) {
        this.slackWebHookUrl = slackWebHookUrl;
    }
    
    /**
     * corp id.
     * @return corp id
     */
    public String getCorpId() {
        return corpId;
    }
    
    /**
     * set corp id.
     * @param corpId corp id
     */
    public void setCorpId(final String corpId) {
        this.corpId = corpId;
    }
    
    /**
     * get agent id.
     * @return agent id
     */
    public String getAgentId() {
        return agentId;
    }
    
    /**
     * set agent id.
     * @param agentId agent id
     */
    public void setAgentId(final String agentId) {
        this.agentId = agentId;
    }
    
    /**
     * get app secret.
     * @return app secret
     */
    public String getAppSecret() {
        return appSecret;
    }
    
    /**
     * set app secret.
     * @param appSecret app secret
     */
    public void setAppSecret(final String appSecret) {
        this.appSecret = appSecret;
    }
    
    /**
     * get discord channel id.
     * @return discord channel id
     */
    public String getDiscordChannelId() {
        return discordChannelId;
    }
    
    /**
     * discord channel id.
     * @param discordChannelId discord channel id
     */
    public void setDiscordChannelId(final String discordChannelId) {
        this.discordChannelId = discordChannelId;
    }
    
    /**
     * discord bot token.
     * @return discord bot token
     */
    public String getDiscordBotToken() {
        return discordBotToken;
    }
    
    /**
     * set discord bot token.
     * @param discordBotToken discord bot token
     */
    public void setDiscordBotToken(final String discordBotToken) {
        this.discordBotToken = discordBotToken;
    }
    
    /**
     * get smn ak.
     * @return smn ak
     */
    public String getSmnAk() {
        return smnAk;
    }
    
    /**
     * set smn ak.
     * @param smnAk smn ak
     */
    public void setSmnAk(final String smnAk) {
        this.smnAk = smnAk;
    }
    
    /**
     * get smn sk.
     * @return smn sk
     */
    public String getSmnSk() {
        return smnSk;
    }
    
    /**
     * set smn sk.
     * @param smnSk smn sk
     */
    public void setSmnSk(final String smnSk) {
        this.smnSk = smnSk;
    }
    
    /**
     * get smn project id.
     * @return smn project id
     */
    public String getSmnProjectId() {
        return smnProjectId;
    }
    
    /**
     * set smn project id.
     * @param smnProjectId smn project id
     */
    public void setSmnProjectId(final String smnProjectId) {
        this.smnProjectId = smnProjectId;
    }
    
    /**
     * smn region.
     * @return smn region
     */
    public String getSmnRegion() {
        return smnRegion;
    }
    
    /**
     * set smn region.
     * @param smnRegion smn region
     */
    public void setSmnRegion(final String smnRegion) {
        this.smnRegion = smnRegion;
    }
    
    /**
     * get smn topic urn.
     * @return smn topic urn
     */
    public String getSmnTopicUrn() {
        return smnTopicUrn;
    }
    
    /**
     * set smn topic urn.
     * @param smnTopicUrn smn topic urn
     */
    public void setSmnTopicUrn(final String smnTopicUrn) {
        this.smnTopicUrn = smnTopicUrn;
    }
    
    /**
     * get labels.
     * @return labels
     */
    public Map<String, String> getLabels() {
        return labels;
    }
    
    /**
     * set labels.
     * @param labels labels
     */
    public void setLabels(final Map<String, String> labels) {
        this.labels = labels;
    }
    
    /**
     * is match all.
     * @return match all
     */
    public boolean isMatchAll() {
        return matchAll;
    }
    
    /**
     * set match all.
     * @param matchAll match all
     */
    public void setMatchAll(final boolean matchAll) {
        this.matchAll = matchAll;
    }
    
    /**
     * get levels.
     * @return levels
     */
    public List<Byte> getLevels() {
        return levels;
    }
    
    /**
     * set levels.
     * @param levels levels
     */
    public void setLevels(final List<Byte> levels) {
        this.levels = levels;
    }
    
    /**
     * get dateCreated.
     *
     * @return dateCreated
     */
    public Date getDateCreated() {
        return dateCreated;
    }
    
    /**
     * set dateCreated.
     *
     * @param dateCreated dateCreated
     */
    public void setDateCreated(final Date dateCreated) {
        this.dateCreated = dateCreated;
    }
    
    /**
     * get dateUpdated.
     *
     * @return dateUpdated
     */
    public Date getDateUpdated() {
        return dateUpdated;
    }
    
    /**
     * set dateUpdated.
     *
     * @param dateUpdated dateUpdated
     */
    public void setDateUpdated(final Date dateUpdated) {
        this.dateUpdated = dateUpdated;
    }
    
}
