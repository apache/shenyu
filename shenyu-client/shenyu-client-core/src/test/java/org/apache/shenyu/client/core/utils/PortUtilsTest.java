package org.apache.shenyu.client.core.utils;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.support.DefaultListableBeanFactory;
import org.springframework.beans.factory.support.GenericBeanDefinition;
import org.springframework.boot.web.embedded.tomcat.TomcatServletWebServerFactory;

/**
 * Test for {@link PortUtils}.
 */
public class PortUtilsTest {

    @Test
    public void testFindPort() {
        DefaultListableBeanFactory beanFactory = new DefaultListableBeanFactory();
        GenericBeanDefinition beanDefinition = new GenericBeanDefinition();
        beanDefinition.setBeanClass(TomcatServletWebServerFactory.class);
        beanFactory.registerBeanDefinition("webServerFactory", beanDefinition);

        int port = PortUtils.findPort(beanFactory);
        Assertions.assertEquals(8080, port);
    }

}
