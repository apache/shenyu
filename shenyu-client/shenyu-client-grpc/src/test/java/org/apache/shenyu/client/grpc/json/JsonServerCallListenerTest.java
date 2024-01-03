package org.apache.shenyu.client.grpc.json;

import org.apache.shenyu.client.grpc.TestRequest;
import org.apache.shenyu.client.grpc.TestResponse;
import org.junit.jupiter.api.Test;

import static org.mockito.Mockito.mock;

public class JsonServerCallListenerTest {
    final private JsonServerCallListener<TestRequest, TestResponse> testJsonServerCallListener = mock(JsonServerCallListener.class);

    @Test
    public void testOnMessage() {
        testJsonServerCallListener.onMessage(new TestRequest("test-on-message"));
    }
}
