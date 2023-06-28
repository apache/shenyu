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

package org.apache.shenyu.alert.model;

import java.io.Serializable;
import java.util.Date;
import java.util.Map;

/**
 * AlertReceiver.
 */
public class AlertReceiverDTO implements Serializable {

    /**
     * primary key id.
     */
    private Long id;

    /**
     * receiver name
     */
    private String name;
	
	/**
	 * is enabled this receiver
	 */
	private boolean enable = true;
	
	/**
	 * Notification information method: 0-SMS 1-Email 2-webhook 3-WeChat Official Account 4-Enterprise WeChat Robot 5-DingTalk Robot 6-FeiShu Robot 7-Telegram Bot 8-SlackWebHook 9-Discord Bot 10-Enterprise WeChat app message
	 */
	private Byte type;
	
	/**
	 * Mobile number: Valid when the notification method is SMS
	 */
	private String phone;
	
	/**
	 * Email account: Valid when the notification method is email
	 */
	private String email;
	
	/**
	 * URL address: The notification method is valid for webhook
	 */
	private String hookUrl;
	
	/**
	 * The notification method is valid for WeChat official account, enterprise WeChat robot or FlyBook robot
	 */
	private String wechatId;
	
	/**
	 * Access token : The notification method is valid for DingTalk robot
	 */
	private String accessToken;
	
	/**
	 * Telegram bot token : The notification method is valid for Telegram Bot
	 */
	private String tgBotToken;
	
	/**
	 * Telegram user id: The notification method is valid for Telegram Bot
	 */
	private String tgUserId;
	
	/**
	 * URL address: The notification method is valid for Slack
	 */
	private String slackWebHookUrl;
	
	/**
	 * Enterprise weChat message: The notification method is valid for Enterprise WeChat app message
	 */
	private String corpId;
	
	/**
	 * Enterprise weChat appId: The notification method is valid for Enterprise WeChat app message
	 */
	private Integer agentId;
	
	/**
	 * Enterprise weChat secret: The notification method is valid for Enterprise WeChat app message
	 */
	private String appSecret;
	
	/**
	 * Discord channel id: The notification method is valid for Discord
	 */
	private String discordChannelId;
	
	/**
	 * Discord bot token: The notification method is valid for Discord
	 */
	private String discordBotToken;
	
	/**
	 * huawei cloud SMN ak: If the notification method is valid for huawei cloud SMN
	 */
	private String smnAk;
	
	/**
	 * huawei cloud SMN sk: If the notification method is valid for huawei cloud SMN
	 */
	private String smnSk;
	
	/**
	 * huawei cloud SMN projectId: If the notification method is valid for huawei cloud SMN
	 */
	private String smnProjectId;
	
	/**
	 * huawei cloud SMN region: If the notification method is valid for huawei cloud SMN
	 */
	private String smnRegion;
	
	/**
	 * huawei cloud SMN TopicUrn: If the notification method is valid for huawei cloud SMN
	 */
	private String smnTopicUrn;
	
	/**
	 * alert labels
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
    public Long getId() {
        return id;
    }

    /**
     * set id.
     * @param id id
     */
    public void setId(final Long id) {
        this.id = id;
    }
	
	public String getName() {
		return name;
	}
	
	public void setName(String name) {
		this.name = name;
	}
	
	public boolean isEnable() {
		return enable;
	}
	
	public void setEnable(boolean enable) {
		this.enable = enable;
	}
	
	public Byte getType() {
		return type;
	}
	
	public void setType(Byte type) {
		this.type = type;
	}
	
	public String getPhone() {
		return phone;
	}
	
	public void setPhone(String phone) {
		this.phone = phone;
	}
	
	public String getEmail() {
		return email;
	}
	
	public void setEmail(String email) {
		this.email = email;
	}
	
	public String getHookUrl() {
		return hookUrl;
	}
	
	public void setHookUrl(String hookUrl) {
		this.hookUrl = hookUrl;
	}
	
	public String getWechatId() {
		return wechatId;
	}
	
	public void setWechatId(String wechatId) {
		this.wechatId = wechatId;
	}
	
	public String getAccessToken() {
		return accessToken;
	}
	
	public void setAccessToken(String accessToken) {
		this.accessToken = accessToken;
	}
	
	public String getTgBotToken() {
		return tgBotToken;
	}
	
	public void setTgBotToken(String tgBotToken) {
		this.tgBotToken = tgBotToken;
	}
	
	public String getTgUserId() {
		return tgUserId;
	}
	
	public void setTgUserId(String tgUserId) {
		this.tgUserId = tgUserId;
	}
	
	public String getSlackWebHookUrl() {
		return slackWebHookUrl;
	}
	
	public void setSlackWebHookUrl(String slackWebHookUrl) {
		this.slackWebHookUrl = slackWebHookUrl;
	}
	
	public String getCorpId() {
		return corpId;
	}
	
	public void setCorpId(String corpId) {
		this.corpId = corpId;
	}
	
	public Integer getAgentId() {
		return agentId;
	}
	
	public void setAgentId(Integer agentId) {
		this.agentId = agentId;
	}
	
	public String getAppSecret() {
		return appSecret;
	}
	
	public void setAppSecret(String appSecret) {
		this.appSecret = appSecret;
	}
	
	public String getDiscordChannelId() {
		return discordChannelId;
	}
	
	public void setDiscordChannelId(String discordChannelId) {
		this.discordChannelId = discordChannelId;
	}
	
	public String getDiscordBotToken() {
		return discordBotToken;
	}
	
	public void setDiscordBotToken(String discordBotToken) {
		this.discordBotToken = discordBotToken;
	}
	
	public String getSmnAk() {
		return smnAk;
	}
	
	public void setSmnAk(String smnAk) {
		this.smnAk = smnAk;
	}
	
	public String getSmnSk() {
		return smnSk;
	}
	
	public void setSmnSk(String smnSk) {
		this.smnSk = smnSk;
	}
	
	public String getSmnProjectId() {
		return smnProjectId;
	}
	
	public void setSmnProjectId(String smnProjectId) {
		this.smnProjectId = smnProjectId;
	}
	
	public String getSmnRegion() {
		return smnRegion;
	}
	
	public void setSmnRegion(String smnRegion) {
		this.smnRegion = smnRegion;
	}
	
	public String getSmnTopicUrn() {
		return smnTopicUrn;
	}
	
	public void setSmnTopicUrn(String smnTopicUrn) {
		this.smnTopicUrn = smnTopicUrn;
	}
	
	public Map<String, String> getLabels() {
		return labels;
	}
	
	public void setLabels(Map<String, String> labels) {
		this.labels = labels;
	}
	
	/**
     * get dateCreated.
     * @return dateCreated
     */
    public Date getDateCreated() {
        return dateCreated;
    }

    /**
     * set dateCreated.
     * @param dateCreated dateCreated
     */
    public void setDateCreated(final Date dateCreated) {
        this.dateCreated = dateCreated;
    }

    /**
     * get dateUpdated.
     * @return dateUpdated
     */
    public Date getDateUpdated() {
        return dateUpdated;
    }

    /**
     * set dateUpdated.
     * @param dateUpdated dateUpdated
     */
    public void setDateUpdated(final Date dateUpdated) {
        this.dateUpdated = dateUpdated;
    }

}
