package org.dromara.soul.sync.data.nacos.config;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;

@Data
public class NacosConfig {
    
    private String url;
    
    private String namespace;
    
    private NacosACMConfig acm;
}
