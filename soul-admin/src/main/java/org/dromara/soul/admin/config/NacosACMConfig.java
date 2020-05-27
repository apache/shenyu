package org.dromara.soul.admin.config;

import lombok.Data;

@Data
public class NacosACMConfig {
    
    private boolean enabled;
    
    private String endpoint;
    
    private String namespace;
    
    private String accessKey;
    
    private String secretKey;
}
