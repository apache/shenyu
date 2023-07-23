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
