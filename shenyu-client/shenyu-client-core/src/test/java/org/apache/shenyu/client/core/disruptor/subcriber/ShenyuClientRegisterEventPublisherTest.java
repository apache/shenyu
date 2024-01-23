package org.apache.shenyu.client.core.disruptor;

import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.type.DataTypeParent;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;

import static org.junit.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertSame;

class ShenyuClientRegisterEventPublisherTest {

    @Mock
    DataTypeParent testData;
    
    @Mock
    ShenyuClientRegisterRepository mockRepository;

    @Test
    void testGetInstanceReturnsSameInstance() {
        ShenyuClientRegisterEventPublisher instance1 = ShenyuClientRegisterEventPublisher.getInstance();
        ShenyuClientRegisterEventPublisher instance2 = ShenyuClientRegisterEventPublisher.getInstance();
        assertSame(instance1, instance2);
    }

    @Test
    void testStart() {
        ShenyuClientRegisterEventPublisher publisher = ShenyuClientRegisterEventPublisher.getInstance();
        publisher.start(mockRepository);
        assertNotNull(publisher.getProviderManage());
        assertDoesNotThrow(() -> publisher.getProviderManage().startup());
    }

    @Test
    void testPublishEvent() {
        ShenyuClientRegisterEventPublisher publisher = ShenyuClientRegisterEventPublisher.getInstance();
        publisher.start(mockRepository);
        assertDoesNotThrow(() -> publisher.publishEvent(testData));
    }

    @Test
    void testPublishEventWithNullData() {
        ShenyuClientRegisterEventPublisher publisher = ShenyuClientRegisterEventPublisher.getInstance();
        publisher.start(mockRepository);
        assertDoesNotThrow(() -> publisher.publishEvent(null));
    }
}
