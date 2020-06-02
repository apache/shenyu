package org.dromara.soul.client.dubbo.common.config;

import lombok.Data;

/**
 * The type Dubbo config.
 *
 * @author xiaoyu
 */
@Data
public class DubboConfig {

    private String adminUrl;

    private String contextPath;

    private String appName;
}
