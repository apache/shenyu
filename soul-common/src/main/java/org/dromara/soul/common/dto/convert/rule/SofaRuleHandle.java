package org.dromara.soul.common.dto.convert.rule;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import org.dromara.soul.common.constant.Constants;

import java.io.Serializable;

/**
 * The type Sofa rule handle.
 *
 * @author tydhot
 */
@Getter
@Setter
@ToString
public class SofaRuleHandle implements Serializable {

    /**
     * retries.
     */
    private Integer retries;

    /**
     * the loadBalance.
     * {@linkplain org.dromara.soul.common.enums.LoadBalanceEnum}
     */
    private String loadBalance;

    /**
     * timeout is required.
     */
    private long timeout = Constants.TIME_OUT;
}
