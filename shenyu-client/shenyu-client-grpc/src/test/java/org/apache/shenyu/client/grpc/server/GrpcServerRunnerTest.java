package org.apache.shenyu.client.grpc.server;

import io.grpc.ServerBuilder;
import org.apache.shenyu.client.grpc.GrpcClientEventListener;
import org.junit.jupiter.api.Test;
import org.springframework.context.event.ContextRefreshedEvent;

import static org.mockito.Mockito.mock;

public class GrpcServerRunnerTest {
    private final ContextRefreshedEvent testEvent = mock(ContextRefreshedEvent.class);
    private final GrpcClientEventListener testGrpcClientEventListener = mock(GrpcClientEventListener.class);
    @Test
    public void testOnApplicationEvent() {
        GrpcServerBuilder testGrpcServerBuilder = () -> ServerBuilder.forPort(8088);

        GrpcServerRunner testGrpcServerRunner = new GrpcServerRunner(testGrpcServerBuilder,testGrpcClientEventListener);

        testGrpcServerRunner.onApplicationEvent(testEvent);
    }
}
