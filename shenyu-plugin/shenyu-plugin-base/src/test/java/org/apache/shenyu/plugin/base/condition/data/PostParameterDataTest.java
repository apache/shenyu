package org.apache.shenyu.plugin.base.condition.data;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.HttpMethodEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;

import java.net.InetSocketAddress;

/**
 * Test cases for {@link PostParameterData}.
 */
public class PostParameterDataTest {

    private ServerWebExchange exchange;

    private PostParameterData postParameterData;

    private ShenyuContext context;

    @Before
    public void setUp() {
        this.exchange = MockServerWebExchange.from(MockServerHttpRequest.get("/uri/path")
                .remoteAddress(new InetSocketAddress("localhost", 8080))
                .build());
        this.context = new ShenyuContext();
        this.context.setRpcType(RpcTypeEnum.HTTP.getName());
        this.context.setHttpMethod(HttpMethodEnum.POST.getName());
        this.exchange.getAttributes().put(Constants.CONTEXT, this.context);
        this.postParameterData = new PostParameterData();
    }

    @Test
    public void testBuilder() {
        Assert.assertEquals(this.postParameterData.builder("httpMethod", this.exchange), "post");
        Assert.assertEquals(this.postParameterData.builder("rpcType", this.exchange), "http");
    }
}
