package org.apache.shenyu.web.filter;

import org.apache.shenyu.common.utils.ShaUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.reactive.DispatcherHandler;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.WebFilterChain;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * test case for LocalDispatcherFilter
 * @see LocalDispatcherFilter
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class LocalDispatcherFilterTest {
    private LocalDispatcherFilter localDispatcherFilter;

    private WebFilterChain webFilterChain;

    @BeforeEach
    public void setup() {
        DispatcherHandler dispatcherHandler = mock(DispatcherHandler.class);
        when(dispatcherHandler.handle(any())).thenReturn(Mono.empty());
        String sha512Key = ShaUtils.shaEncryption("123456");
        localDispatcherFilter = new LocalDispatcherFilter(dispatcherHandler, sha512Key);
        webFilterChain = mock(WebFilterChain.class);
        when(webFilterChain.filter(any())).thenReturn(Mono.empty());
    }

    /**
     * test method for {@linkplain LocalDispatcherFilter#doFilter(ServerWebExchange, WebFilterChain)}
     * execute {@linkplain LocalDispatcherFilter#doDenyResponse(ServerWebExchange)}
     */
    @Test
    public void testFilter() {
        ServerWebExchange serverWebExchange =
                MockServerWebExchange.from(MockServerHttpRequest
                        .post("http://localhost:8080/shenyu/test")
                        .header("localKey", "123456"));
        Mono<Void> filter = localDispatcherFilter.filter(serverWebExchange, webFilterChain);
        StepVerifier.create(filter).expectSubscription().verifyComplete();
    }

    /**
     * test method for {@linkplain LocalDispatcherFilter#doFilter(ServerWebExchange, WebFilterChain)}
     */
    @Test
    public void testFilterNotMatch() {
        ServerWebExchange webExchange =
                MockServerWebExchange.from(MockServerHttpRequest
                        .post("http://localhost:8080/test"));
        Mono<Void> filter = localDispatcherFilter.filter(webExchange, webFilterChain);
        StepVerifier.create(filter).expectSubscription().verifyComplete();
    }
}
