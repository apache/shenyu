package org.apache.shenyu.plugin.basic.auth;

import java.util.Base64;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.result.ShenyuResultEnum;
import org.apache.shenyu.plugin.api.result.ShenyuResultWrap;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.basic.auth.config.BasicAuthConfig;
import org.springframework.http.HttpHeaders;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

/**
 * @author romic
 * @date 2022/7/18
 */
public class BasicAuthPlugin extends AbstractShenyuPlugin {
    /**
     * this is Template Method child has Implement your own logic.
     *
     * @param exchange exchange the current server exchange {@linkplain ServerWebExchange}
     * @param chain    chain the current chain  {@linkplain ServerWebExchange}
     * @param selector selector    {@linkplain SelectorData}
     * @param rule     rule    {@linkplain RuleData}
     * @return {@code Mono<Void>} to indicate when request handling is complete
     */
    @Override
    protected Mono<Void> doExecute(ServerWebExchange exchange, ShenyuPluginChain chain, SelectorData selector, RuleData rule) {
        BasicAuthConfig basicAuthConfig = Singleton.INST.get(BasicAuthConfig.class);
        String authorization = exchange.getRequest().getHeaders().getFirst(HttpHeaders.AUTHORIZATION);
        String[] userAndPass = new String(Base64.getDecoder().decode(authorization.split(" ")[1])).split(":");

        // check authorization
        if (userAndPass.length < 2) {
            Object error = ShenyuResultWrap.error(exchange, ShenyuResultEnum.SECRET_KEY_MUST_BE_CONFIGURED);
            return WebFluxResultUtils.result(exchange, error);
        }

        if (StringUtils.equals(basicAuthConfig.getUsername(), userAndPass[0]) && StringUtils.equals(basicAuthConfig.getPassword(), userAndPass[1])) {
            return chain.execute(exchange);
        }
        return WebFluxResultUtils.result(exchange, ShenyuResultWrap.error(exchange, ShenyuResultEnum.ERROR_TOKEN));
    }

    @Override
    public String named() {
        return PluginEnum.BASIC_AUTH.getName();
    }

    @Override
    public int getOrder() {
        return PluginEnum.BASIC_AUTH.getCode();
    }
}
