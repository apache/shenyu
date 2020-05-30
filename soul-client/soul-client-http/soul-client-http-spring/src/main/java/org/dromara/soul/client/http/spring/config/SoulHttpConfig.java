package org.dromara.soul.client.http.spring.config;

import lombok.Data;

/**
 * The type Soul http config.
 *
 * @author xiaoyu
 */
@Data
public class SoulHttpConfig {
    
    private String adminUrl;
    
    private String contextPath;
    
    private String appName;
    
    private String host;
    
    private Integer port;
}
