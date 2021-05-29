package org.apache.shenyu.plugin.base.condition.data;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;

import java.net.InetSocketAddress;

/**
 * Test cases for {@link RequestMethodParameterData}.
 */
public class RequestMethodParameterDataTest {

    private ServerWebExchange exchange;

    private RequestMethodParameterData requestMethodParameterData;

    @Before
    public void setUp() {
        this.exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/uri/path")
                .remoteAddress(new InetSocketAddress("localhost", 8080))
                .build());
        this.requestMethodParameterData = new RequestMethodParameterData();
    }

    @Test
    public void testBuilder() {
        Assert.assertEquals(this.requestMethodParameterData.builder(null, this.exchange), "GET");
    }
}
