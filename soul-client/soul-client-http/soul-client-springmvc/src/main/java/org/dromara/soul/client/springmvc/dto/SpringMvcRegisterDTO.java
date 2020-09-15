package org.dromara.soul.client.springmvc.dto;

import lombok.Builder;
import lombok.Data;

/**
 * The type Spring mvc register dto.
 *
 * @author xiaoyu
 */
@Data
@Builder
public class SpringMvcRegisterDTO {
    
    private String appName;
    
    private String context;
    
    private String path;
    
    private String pathDesc;
    
    private String rpcType;
    
    private String host;
    
    private Integer port;
    
    private String ruleName;
    
    private boolean enabled;
    
    private boolean registerMetaData;
}
