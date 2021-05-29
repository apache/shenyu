package org.apache.shenyu.plugin.base.condition.data;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;

import java.net.InetSocketAddress;

/**
 * Test cases for {@link QueryParameterData}.
 */
public class QueryParameterDataTest {

    private ServerWebExchange exchange;

    private QueryParameterData queryParameterData;

    @Before
    public void setUp() {
        this.exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/uri/path")
                .queryParam("key", "value")
                .remoteAddress(new InetSocketAddress("localhost", 8080))
                .build());
        this.queryParameterData = new QueryParameterData();
    }

    @Test
    public void testBuilder() {
        Assert.assertEquals(this.queryParameterData.builder("key", this.exchange), "value");
    }
}
