package org.dromara.soul.configuration.nacos;

import org.springframework.boot.context.properties.ConfigurationProperties;

import lombok.Data;

@Data
@ConfigurationProperties(prefix="soul.sync.nacos")
public class NacosConfig {
	private String url;
	private String namespace;
	private NacosACMConfig acm;
}
