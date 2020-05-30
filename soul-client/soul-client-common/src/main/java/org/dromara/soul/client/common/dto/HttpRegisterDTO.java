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
public class HttpRegisterDTO {
    
    private String appName;
    
    private String context;
    
    private String path;
    
    private String pathDesc;
    
    private String rpcType;
    
    private String serviceName;
    
    private String methodName;
    
    private String host;
    
    private Integer port;
    
    private Boolean writeMetaData;
    
    private String ruleName;
    
    private boolean enabled;
    
    
}
