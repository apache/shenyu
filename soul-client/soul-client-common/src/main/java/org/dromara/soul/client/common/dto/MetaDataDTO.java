package org.dromara.soul.client.common.dto;

import lombok.Builder;
import lombok.Data;

/**
 * The type Meta data dto.
 *
 * @author xiaoyu
 */
@Data
@Builder
public class MetaDataDTO {

    private String appName;

    private String path;

    private String pathDesc;

    private String rpcType;

    private String serviceName;

    private String methodName;

    private String parameterTypes;

    private String rpcExt;

    private boolean enabled;


    /**
     * The type Rpc ext.
     */
    @Data
    @Builder
    public static class RpcExt {

        private String group;

        private String version;

        private String loadbalance;

        private Integer retries;

        private Integer timeout;

    }


}
