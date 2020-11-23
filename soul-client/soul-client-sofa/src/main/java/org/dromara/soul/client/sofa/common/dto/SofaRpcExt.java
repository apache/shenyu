package org.dromara.soul.client.sofa.common.dto;

import lombok.Builder;
import lombok.Data;

/**
 * The type Sofa rpc ext.
 *
 * @author tydhot
 */
@Data
@Builder
public class SofaRpcExt {

    private String loadbalance;

    private Integer retries;

    private Integer timeout;
}
