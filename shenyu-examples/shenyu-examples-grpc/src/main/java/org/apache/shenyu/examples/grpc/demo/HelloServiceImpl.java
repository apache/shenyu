package org.apache.shenyu.examples.grpc.demo;

import hello.HelloRequest;
import hello.HelloResponse;
import hello.HelloServiceGrpc;
import io.grpc.stub.StreamObserver;
import org.apache.shenyu.client.grpc.common.annotation.ShenyuGrpcClient;
import org.springframework.stereotype.Service;


@ShenyuGrpcClient(path = "/helloService/**", desc = "hello")
@Service
public class HelloServiceImpl extends HelloServiceGrpc.HelloServiceImplBase {

    @Override
    public void hello(HelloRequest request, StreamObserver<HelloResponse> responseObserver) {
        HelloResponse response = HelloResponse.newBuilder().setData("hello: " + request.getData()).build();
        responseObserver.onNext(response);
        responseObserver.onCompleted();
    }

    @Override
    public StreamObserver<HelloRequest> helloEveryOne(StreamObserver<HelloResponse> responseObserver) {
        return new StreamObserver<HelloRequest>() {
            @Override
            public void onNext(HelloRequest request) {
                HelloResponse responseData = HelloResponse.newBuilder()
                        .setData("hello: " + request.getData())
                        .build();
                responseObserver.onNext(responseData);
            }

            @Override
            public void onError(final Throwable t) {
                t.printStackTrace();
            }

            @Override
            public void onCompleted() {
                HelloResponse responseData = HelloResponse.newBuilder()
                        .setData("hello onCompleted")
                        .build();
                responseObserver.onNext(responseData);
                responseObserver.onCompleted();
            }
        };
    }
}
