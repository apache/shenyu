package org.apache.shenyu.examples.grpc.demo;


import event.EventRequest;
import event.EventResponse;
import event.EventServiceGrpc;
import io.grpc.stub.StreamObserver;
import org.apache.shenyu.client.grpc.common.annotation.ShenyuGrpcClient;
import org.springframework.stereotype.Service;

@ShenyuGrpcClient(path = "/eventService", desc = "event")
@Service
public class EventServiceImpl extends EventServiceGrpc.EventServiceImplBase {

    @ShenyuGrpcClient(path = "/sendEvent", desc = "sendEvent")
    @Override
    public void sendEvent(EventRequest request, StreamObserver<EventResponse> responseObserver) {
        EventResponse response = EventResponse.newBuilder().setData("received event:" + request.getData()).build();
        responseObserver.onNext(response);
        responseObserver.onCompleted();
    }

    @ShenyuGrpcClient(path = "/sendEventStream", desc = "sendEventStream")
    @Override
    public StreamObserver<EventRequest> sendEventStream(StreamObserver<EventResponse> responseObserver) {
        return new StreamObserver<EventRequest>() {
            @Override
            public void onNext(EventRequest request) {
                EventResponse responseData = EventResponse.newBuilder()
                        .setData("received event:" + request.getData())
                        .build();
                responseObserver.onNext(responseData);
            }

            @Override
            public void onError(final Throwable t) {
                t.printStackTrace();
            }

            @Override
            public void onCompleted() {
                EventResponse responseData = EventResponse.newBuilder()
                        .setData("event onCompleted")
                        .build();
                responseObserver.onNext(responseData);
                responseObserver.onCompleted();
            }
        };
    }
}
