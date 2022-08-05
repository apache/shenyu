package org.apache.shenyu.client.springmvc.init;

import org.apache.shenyu.client.core.constant.ShenyuClientConstants;
import org.apache.shenyu.client.core.disruptor.ShenyuClientRegisterEventPublisher;
import org.apache.shenyu.client.core.exception.ShenyuClientIllegalArgumentException;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.PortUtils;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.config.PropertiesConfig;
import org.junit.Assert;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.core.env.Environment;

import java.util.Properties;

import static org.mockito.Mockito.*;

/**
 * Test for {@link ContextRegisterListener}.
 */
@ExtendWith(MockitoExtension.class)
public class ContextRegisterListenerTest {

    private ContextRegisterListener contextRegisterListener;

    @Mock
    private PropertiesConfig config;

    @Mock
    private Properties properties;

    @Mock
    private BeanFactory beanFactory;

    @BeforeEach
    public void beforeEach() {
        when(config.getProps()).thenReturn(properties);
        when(properties.getProperty(ShenyuClientConstants.IS_FULL, Boolean.FALSE.toString())).thenReturn(Boolean.TRUE.toString());

        // hit throws
        Assert.assertThrows(ShenyuClientIllegalArgumentException.class, () -> new ContextRegisterListener(config));

        // hit success
        when(properties.getProperty(ShenyuClientConstants.CONTEXT_PATH)).thenReturn(ShenyuClientConstants.CONTEXT_PATH);
        contextRegisterListener = new ContextRegisterListener(config);

        ShenyuClientRegisterEventPublisher publisher = ShenyuClientRegisterEventPublisher.getInstance();
        publisher.start(mock(ShenyuClientRegisterRepository.class));
    }

    @Test
    public void testSetBeanFactory() {
        contextRegisterListener.setBeanFactory(beanFactory);
    }

    @Test
    public void testOnApplicationEvent() {
        MockedStatic<PortUtils> portUtilsMockedStatic = mockStatic(PortUtils.class);
        portUtilsMockedStatic.when(() -> PortUtils.findPort(beanFactory)).thenReturn(8080);
        contextRegisterListener.onApplicationEvent(mock(ContextRefreshedEvent.class));

        // hit `!registered.compareAndSet(false, true)`
        contextRegisterListener.onApplicationEvent(mock(ContextRefreshedEvent.class));
        portUtilsMockedStatic.close();
    }

    @Test
    public void testOnApplicationEventError() {
        Assert.assertThrows(ShenyuException.class, () -> contextRegisterListener.onApplicationEvent(mock(ContextRefreshedEvent.class)));
    }
}
