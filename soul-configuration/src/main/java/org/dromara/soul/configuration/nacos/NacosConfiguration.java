package org.dromara.soul.configuration.nacos;

import java.util.Properties;

import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;

import com.alibaba.nacos.api.NacosFactory;
import com.alibaba.nacos.api.PropertyKeyConst;
import com.alibaba.nacos.api.config.ConfigService;

/**
 * NacosConfiguration
 * 
 * @author Chenxj
 * @date 2020年3月12日-下午12:05:50
 */
@EnableConfigurationProperties(NacosConfig.class)
public class NacosConfiguration {
	
	/**
	 * register configService in spring ioc.
	 * 
	 * @param nacosConfig the nacos configuration
	 * @return ConfigService {@linkplain ConfigService}
	 * @throws Exception
	 * @date 2020年3月12日 下午1:44:55
	 */
	@Bean
	public ConfigService nacosConfigService(final NacosConfig nacosConfig) throws Exception {
		Properties properties=new Properties();
		if(nacosConfig.getAcm()!=null&&nacosConfig.getAcm().isEnabled()) {
			//使用阿里云ACM服务
			properties.put(PropertyKeyConst.ENDPOINT, nacosConfig.getAcm().getEndpoint());
			properties.put(PropertyKeyConst.NAMESPACE, nacosConfig.getAcm().getNamespace());
			//使用子账户ACM管理权限
			properties.put(PropertyKeyConst.ACCESS_KEY, nacosConfig.getAcm().getAccessKey());
			properties.put(PropertyKeyConst.SECRET_KEY, nacosConfig.getAcm().getSecretKey());
		}else {
			properties.put(PropertyKeyConst.SERVER_ADDR, nacosConfig.getUrl());
			properties.put(PropertyKeyConst.NAMESPACE, nacosConfig.getNamespace());
		}
		return NacosFactory.createConfigService(properties);
	}
}
