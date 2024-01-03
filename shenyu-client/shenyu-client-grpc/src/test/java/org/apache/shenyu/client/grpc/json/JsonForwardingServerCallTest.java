package org.apache.shenyu.client.grpc.json;

import org.apache.shenyu.client.grpc.TestRequest;
import org.apache.shenyu.client.grpc.TestResponse;
import org.junit.jupiter.api.Test;

import static org.mockito.Mockito.mock;

public class JsonForwardingServerCallTest {
    private final JsonForwardingServerCall<TestRequest, TestResponse> testJsonForwardingServerCall = mock(JsonForwardingServerCall.class);

    @Test
    public void sentMsgTest() {
        testJsonForwardingServerCall.sendMessage(new TestResponse("test-response"));
    }
}
