package org.dromara.soul.web.spring;

import org.dromara.soul.web.plugin.SoulPlugin;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.stereotype.Component;

/**
 * SoulPluginBeanPostProcessor
 * 
 * @author Chenxj
 * @date 2020年3月13日-下午4:59:09
 */
@Component
public class SoulPluginBeanPostProcessor implements BeanPostProcessor{
	private Logger log=LoggerFactory.getLogger(getClass());
	@Override
	public Object postProcessAfterInitialization(Object bean, String beanName) throws BeansException {
		if(bean instanceof SoulPlugin) {
			SoulPlugin sp=(SoulPlugin)bean;
			log.info("find pluging:[{}]\t[ {} ]",sp.named(),sp.getClass().getName());
		}
		return bean;
	}
}
