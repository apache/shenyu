package org.dromara.soul.client.springmvc.config;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * The type Soul http config.
 *
 * @author xiaoyu
 */
@Data
@ConfigurationProperties(prefix = "soul.http")
public class SoulHttpConfig {

    private String adminUrl;

    private String zookeeperUrl;

    private String contextPath;

    private String appName;
}
