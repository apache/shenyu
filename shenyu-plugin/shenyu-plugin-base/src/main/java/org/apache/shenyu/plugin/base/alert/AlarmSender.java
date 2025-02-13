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

package org.apache.shenyu.plugin.base.alert;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.dto.AlarmContent;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;

import java.util.Date;
import java.util.Map;
import java.util.Objects;


/**
 * The alarm data sender.
 * We can use <code>AlarmSender.alarm</code> to send alarm message.
 * <pre>{@code 
 * AlarmSender.alarm((byte) 0, "alarm-title", "alarm-content");
 *
 * Map<String, String> labels = new HashMap<>(8);
 * labels.put("plugin", "http-redirect");
 * labels.put("component", "http");
 * labels.put("env", "prod");
 * AlarmSender.alarmHighEmergency("alarm-title", "alarm-content", labels);
 * AlarmSender.alarmMediumCritical("alarm-title", "alarm-content", labels);
 * AlarmSender.alarmLowWarning("alarm-title", "alarm-content", labels);
 * 
 * Map<String, String> labels = new HashMap<>(8);
 * labels.put("plugin", "cache");
 * labels.put("component", "cache");
 * labels.put("env", "test");
 * AlarmSender.alarm((byte) 0, "alarm-title", "alarm-content", labels);
 * }</pre>
 * 
 */
public class AlarmSender {
    
    private static AlarmService alarmService;
    
    private static Boolean enabled;

    private static String namespaceId;
    
    /**
     * Send alarm content.
     * @param alarmContent alarm content
     */
    public static void alarm(final AlarmContent alarmContent) {
        if (StringUtils.isNotEmpty(alarmContent.getNamespaceId())) {
            ShenyuConfig shenyuConfig = SpringBeanUtils.getInstance().getBean(ShenyuConfig.class);
            namespaceId = shenyuConfig.getNamespace();
            alarmContent.setNamespaceId(namespaceId);
        }
        if (Objects.isNull(alarmService)) {
            alarmService = SpringBeanUtils.getInstance().getBean(AlarmService.class);
        }
        if (Objects.isNull(enabled)) {
            ShenyuConfig shenyuConfig = SpringBeanUtils.getInstance().getBean(ShenyuConfig.class);
            enabled = shenyuConfig.getAlert().getEnabled();
        }
        AlarmThreadPoolExecutor.getInstance().execute(() -> alarmService.alarm(alarmContent));
    }

    /**
     * Send alarm content.
     * @param level Alarm level. 0: high-emergency-critical 1: medium-critical-critical 2: low-warning-warning
     * @param title Alarm title
     * @param content Alarm content
     * @param labels Alarm labels
     */
    public static void alarm(final byte level, final String title, final String content, final Map<String, String> labels) {
        AlarmContent alarmContent = new AlarmContent.Builder()
                                            .level(level).title(title).content(content)
                                            .labels(labels).namespaceId(namespaceId)
                                            .dateCreated(new Date()).build();
        alarm(alarmContent);
    }

    /**
     * Send alarm content.
     * @param level Alarm level. 0: high-emergency-critical 1: medium-critical-critical 2: low-warning-warning
     * @param title Alarm title
     * @param content Alarm content
     */
    public static void alarm(final byte level, final String title, final String content) {
        AlarmContent alarmContent = new AlarmContent.Builder()
                                            .level(level).title(title)
                                            .content(content).namespaceId(namespaceId)
                                            .dateCreated(new Date()).build();
        alarm(alarmContent);
    }

    /**
     * Send high emergency level alarm content.
     * @param title Alarm title
     * @param content Alarm content
     * @param labels Alarm labels
     */
    public static void alarmHighEmergency(final String title, final String content, final Map<String, String> labels) {
        AlarmContent alarmContent = new AlarmContent.Builder()
                                            .level((byte) 0).title(title).content(content)
                                            .labels(labels).namespaceId(namespaceId)
                                            .dateCreated(new Date()).build();
        alarm(alarmContent);
    }

    /**
     * Send medium critical level alarm content.
     * @param title Alarm title
     * @param content Alarm content
     * @param labels Alarm labels
     */
    public static void alarmMediumCritical(final String title, final String content, final Map<String, String> labels) {
        AlarmContent alarmContent = new AlarmContent.Builder()
                                            .level((byte) 1).title(title).content(content)
                                            .labels(labels).namespaceId(namespaceId)
                                            .dateCreated(new Date()).build();
        alarm(alarmContent);
    }

    /**
     * Send low warning level alarm content.
     * @param title Alarm title
     * @param content Alarm content
     * @param labels Alarm labels
     */
    public static void alarmLowWarning(final String title, final String content, final Map<String, String> labels) {
        AlarmContent alarmContent = new AlarmContent.Builder()
                                            .level((byte) 2).title(title).content(content)
                                            .labels(labels).namespaceId(namespaceId)
                                            .dateCreated(new Date()).build();
        alarm(alarmContent);
    }
}
