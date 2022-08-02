package org.apache.shenyu.examples.websocket.handler;

import org.apache.shenyu.examples.common.aop.Log;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Component;
import org.springframework.web.reactive.socket.WebSocketHandler;
import org.springframework.web.reactive.socket.WebSocketSession;
import reactor.core.publisher.Mono;


@Component
public class UploadFileHandler implements WebSocketHandler {


    @Override
    @NonNull
    @Log
    public Mono<Void> handle(final WebSocketSession session) {
        return session.send(
                session.receive()
                        .map(msg -> session.binaryMessage(msg.getNativeMessage())));
    }
}
