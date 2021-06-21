package org.apache.shenyu.plugin.oauth2.filter;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.WebFilterChain;
import reactor.test.StepVerifier;

import java.util.List;

import static org.mockito.Mockito.mock;

@RunWith(MockitoJUnitRunner.class)
public class OAuth2PreFilterTest {

    @Mock
    private OAuth2PreFilter preFilter;

    @Before
    public void setup() {
        preFilter = new OAuth2PreFilter(mock(List.class));
    }

    @Test
    public void testFilter() {
        StepVerifier.create(preFilter.filter(MockServerWebExchange.from(MockServerHttpRequest.get("localhost").build()), mock(WebFilterChain.class)))
            .expectSubscription().verifyComplete();
    }
}
