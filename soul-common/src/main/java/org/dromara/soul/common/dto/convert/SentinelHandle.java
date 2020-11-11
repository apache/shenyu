package org.dromara.soul.common.dto.convert;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.Setter;

/**
 * This is SentinelHandle.
 *
 * @author tydhot
 */
@Getter
@Setter
@EqualsAndHashCode
public class SentinelHandle {

    /**
     * flow rule enable.
     */
    private Integer flowRuleEnable = 1;

    /**
     * flow rule grade.
     */
    private Integer flowRuleGrade = 1;

    /**
     * flow rule count.
     */
    private Integer flowRuleCount;

    /**
     * flow rule control behavior.
     */
    private Integer flowRuleControlBehavior = 0;

    /**
     * degrade rule control behavior.
     */
    private Integer degradeRuleEnable = 1;

    /**
     * degrade rule grade.
     */
    private Integer degradeRuleGrade = 1;

    /**
     * degrade rule count.
     */
    private Integer degradeRuleCount;

    /**
     * degrade rule time window.
     */
    private Integer degradeRuleTimeWindow;
}
