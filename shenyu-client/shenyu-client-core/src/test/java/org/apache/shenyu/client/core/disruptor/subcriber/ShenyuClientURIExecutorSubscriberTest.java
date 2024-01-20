package org.apache.shenyu.client.core.disruptor.subcriber;

import org.apache.shenyu.client.core.shutdown.ShenyuClientShutdownHook;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.common.enums.EventType;
import org.apache.shenyu.register.common.type.DataType;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Properties;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

/**
 * Test for {@link ShenyuClientURIExecutorSubscriber}.
 */
public class ShenyuClientURIExecutorSubscriberTest {


    private ShenyuClientRegisterRepository shenyuClientRegisterRepository;

    private ShenyuClientURIExecutorSubscriber executorSubscriber;

    @BeforeEach
    public void setUp() {
        shenyuClientRegisterRepository = mock(ShenyuClientRegisterRepository.class);
        executorSubscriber = new ShenyuClientURIExecutorSubscriber(shenyuClientRegisterRepository);
        // set properties to avoid NullPointerException
        Properties properties = new Properties();
        ShenyuClientShutdownHook.set(shenyuClientRegisterRepository, properties);
    }

    @Test
    public void testGetType() {
        DataType expected = DataType.URI;
        DataType actual = executorSubscriber.getType();
        assertEquals(expected, actual);
    }

    @Test
    public void testExecutorWithEmptyData() {
        Collection<URIRegisterDTO> dataList = new ArrayList<>();
        executorSubscriber.executor(dataList);

        verify(shenyuClientRegisterRepository, never()).persistApiDoc(any());
    }

    @Test
    public void testExecutorValidData() {

        Collection<URIRegisterDTO> uriRegisterDTOList = new ArrayList<>();

        URIRegisterDTO uriRegisterDTO =
                URIRegisterDTO.builder().protocol("http").contextPath("/test").rpcType("http").host("localhost").eventType(EventType.REGISTER).port(9082).build();
        uriRegisterDTOList.add(uriRegisterDTO);

        executorSubscriber.executor(uriRegisterDTOList);
        verify(shenyuClientRegisterRepository, times(1)).persistURI(uriRegisterDTO);
    }
}
