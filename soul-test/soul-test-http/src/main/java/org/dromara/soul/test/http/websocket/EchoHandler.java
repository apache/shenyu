package org.dromara.soul.test.http.websocket;

import org.springframework.stereotype.Component;
import org.springframework.web.reactive.socket.WebSocketHandler;
import org.springframework.web.reactive.socket.WebSocketSession;
import reactor.core.publisher.Mono;

/**
 * The type Echo handler.
 *
 * @author xiaoyu(Myth)
 */
@Component
public class EchoHandler implements WebSocketHandler {
    @Override
    public Mono<Void> handle(final WebSocketSession session) {
        return session.send(
                session.receive()
                        .map(msg -> session.textMessage(
                                "result xiaoyu， -> " + msg.getPayloadAsText())));
    }
}
