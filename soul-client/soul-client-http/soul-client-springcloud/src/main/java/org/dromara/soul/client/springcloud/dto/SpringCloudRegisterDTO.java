package org.dromara.soul.client.springcloud.dto;

import lombok.Builder;
import lombok.Data;

/**
 * The type Spring cloud register dto.
 *
 * @author xiaoyu
 */
@Data
@Builder
public class SpringCloudRegisterDTO {
    
    private String appName;
    
    private String context;
    
    private String path;
    
    private String pathDesc;
    
    private String rpcType;
    
    private String ruleName;
    
    private boolean enabled;
}
