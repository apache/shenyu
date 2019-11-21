package org.dromara.soul.client.dubbo.config;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * @author xiaoyu
 */
@ConfigurationProperties(prefix = "soul.dubbo")
@Data
public class DubboConfig {

    private String url;

    private String contextPath;

    private String appName;
}
