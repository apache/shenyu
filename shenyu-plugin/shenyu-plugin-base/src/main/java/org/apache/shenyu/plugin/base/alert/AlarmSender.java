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

import org.apache.shenyu.common.dto.AlarmContent;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Map;

/**
 * alarm data sender
 */
public class AlarmSender {
    
    private static final Logger logger = LoggerFactory.getLogger(AlarmSender.class);
    
    private static AlarmService alarmService;
    
    /**
     * send alarm content to shenyu admin
     * @param alarmContent alarm content
     */
    public static void alarm(AlarmContent alarmContent) {
        if (alarmService == null) {
            alarmService = SpringBeanUtils.getInstance().getBean(AlarmService.class);
        }
        AlarmThreadPoolExecutor.getInstance().execute(() -> {
            alarmService.alarm(alarmContent); 
        });
    }
    
    /**
     * send alarm content to shenyu admin
     * @param level Alarm level. 0: high-emergency-critical 1: medium-critical-critical 2: low-warning-warning
     * @param title Alarm title
     * @param content Alarm content
     * @param labels Alarm labels
     */
    public static void alarm(byte level, String title, String content, Map<String, String> labels) {
        AlarmContent alarmContent = new AlarmContent.Builder()
                                            .level(level).title(title).content(content)
                                            .labels(labels).build();
        alarm(alarmContent);
    }
    
    /**
     * send alarm content to shenyu admin
     * @param level Alarm level. 0: high-emergency-critical 1: medium-critical-critical 2: low-warning-warning
     * @param title Alarm title
     * @param content Alarm content
     */
    public static void alarm(byte level, String title, String content) {
        AlarmContent alarmContent = new AlarmContent.Builder()
                                            .level(level).title(title)
                                            .content(content).build();
        alarm(alarmContent);
    }
}
