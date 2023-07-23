package org.apache.shenyu.plugin.base.alert;

import org.apache.shenyu.common.dto.AlarmContent;

/**
 * alarm service
 */
public interface AlarmService {
    
    void alarm(AlarmContent content);
}
