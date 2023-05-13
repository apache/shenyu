package org.apache.shenyu.client.core.disruptor.subcriber;

import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.dto.ApiDocRegisterDTO;
import org.apache.shenyu.register.common.enums.EventType;
import org.apache.shenyu.register.common.type.DataType;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Collection;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

/**
 * Test for {@link ShenyuClientApiDocExecutorSubscriber}.
 */
public class ShenyuClientApiDocExecutorSubscriberTest {

	private ShenyuClientRegisterRepository shenyuClientRegisterRepository;
	private ShenyuClientApiDocExecutorSubscriber executorSubscriber;

	@BeforeEach
	public void setUp() {
		shenyuClientRegisterRepository = mock(ShenyuClientRegisterRepository.class);
		executorSubscriber = new ShenyuClientApiDocExecutorSubscriber(shenyuClientRegisterRepository);
	}

	@Test
	public void testGetType() {
		DataType expected = DataType.API_DOC;
		DataType actual = executorSubscriber.getType();
		assertEquals(expected, actual);
	}

	@Test
	public void testExecutorWithEmptyData() {
		Collection<ApiDocRegisterDTO> dataList = new ArrayList<>();
		executorSubscriber.executor(dataList);

		verify(shenyuClientRegisterRepository, never()).persistApiDoc(any());
	}

	@Test
	public void testExecutorValidData() {
		Collection<ApiDocRegisterDTO> apiDocRegisterDTOList = new ArrayList<>();

		ApiDocRegisterDTO apiDocRegisterDTO = ApiDocRegisterDTO.builder()
				.contextPath("/test")
				.apiPath("/api")
				.httpMethod(0)
				.consume("application/json")
				.produce("application/json")
				.version("V0.01")
				.rpcType("http")
				.state(1)
				.ext("test")
				.apiOwner("test")
				.apiDesc("test")
				.apiSource(0)
				.document("test")
				.eventType(EventType.UPDATED)
				.tags(new ArrayList<>())
				.build();
		apiDocRegisterDTOList.add(apiDocRegisterDTO);

		executorSubscriber.executor(apiDocRegisterDTOList);

		verify(shenyuClientRegisterRepository, times(1)).persistApiDoc(apiDocRegisterDTO);
	}

}