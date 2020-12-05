package org.dromara.soul.plugin.sign;

import org.apache.commons.lang3.tuple.Pair;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.plugin.api.SignService;
import org.dromara.soul.plugin.api.SoulPluginChain;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * @author Phoenix Luo
 * @version 2020/12/5
 **/
@RunWith(MockitoJUnitRunner.class)
public class SignPluginTest {
    @Mock
    private SoulPluginChain chain;
    
    private ServerWebExchange exchange;
    
    private SignPlugin signPlugin;
    
    @Before
    public void setup() {
        this.exchange = MockServerWebExchange.from(MockServerHttpRequest.get("localhost").build());
        SignService signService = mock(SignService.class);
        when(signService.signVerify(exchange)).thenReturn(Pair.of(true, ""));
        this.signPlugin = new SignPlugin(signService);
    }
    
    @Test
    public void testSignPlugin() {
        RuleData data = mock(RuleData.class);
        SelectorData selectorData = mock(SelectorData.class);
        when(chain.execute(exchange)).thenReturn(Mono.empty());
        StepVerifier.create(signPlugin.doExecute(exchange, chain, selectorData, data)).expectSubscription().verifyComplete();
    }
}
