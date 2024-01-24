package org.apache.shenyu.client.core.disruptor.subcriber;

import org.apache.shenyu.client.core.disruptor.ShenyuClientRegisterEventPublisher;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.type.DataTypeParent;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;

import static org.junit.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertSame;

public class ShenyuClientRegisterEventPublisherTest {
    @Mock
    private DataTypeParent testData;
    @Mock
    private ShenyuClientRegisterRepository shenyuClientRegisterRepository;
    private ShenyuClientRegisterEventPublisher publisher;

    @BeforeEach
    public void setUp() {
        publisher = ShenyuClientRegisterEventPublisher.getInstance();
        publisher.start(shenyuClientRegisterRepository);
    }

    @Test
    public void testGetInstanceReturnsSameInstance() {
        ShenyuClientRegisterEventPublisher instance1 = ShenyuClientRegisterEventPublisher.getInstance();
        ShenyuClientRegisterEventPublisher instance2 = ShenyuClientRegisterEventPublisher.getInstance();
        assertSame(instance1, instance2);
    }

    @Test
    public void testStart() {
        ShenyuClientRegisterEventPublisher publisher = ShenyuClientRegisterEventPublisher.getInstance();
        publisher.start(shenyuClientRegisterRepository);
        assertNotNull(publisher.getProviderManage());
        assertDoesNotThrow(() -> publisher.getProviderManage().startup());
    }

    @Test
    public void testPublishEvent() {
        ShenyuClientRegisterEventPublisher publisher = ShenyuClientRegisterEventPublisher.getInstance();
        publisher.start(shenyuClientRegisterRepository);
        assertDoesNotThrow(() -> publisher.publishEvent(testData));
    }

    @Test
    public void testPublishEventWithNullData() {
        ShenyuClientRegisterEventPublisher publisher = ShenyuClientRegisterEventPublisher.getInstance();
        publisher.start(shenyuClientRegisterRepository);
        assertDoesNotThrow(() -> publisher.publishEvent(null));
    }
}
