package org.dromara.soul.plugin.oauth;


import lombok.extern.slf4j.Slf4j;
import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.common.dto.convert.OAuthHandle;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.common.utils.GsonUtils;
import org.dromara.soul.plugin.api.SoulPluginChain;
import org.dromara.soul.plugin.api.context.SoulContext;
import org.dromara.soul.plugin.api.result.SoulResultEnum;
import org.dromara.soul.plugin.base.AbstractSoulPlugin;
import org.dromara.soul.plugin.base.utils.SoulResultWarp;
import org.dromara.soul.plugin.base.utils.WebFluxResultUtils;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.Objects;

@Slf4j
public class OAuthPlugin extends AbstractSoulPlugin {

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final SoulPluginChain chain, final SelectorData selector, RuleData rule) {
        final String ruleHandle = rule.getHandle();
        if (Objects.isNull(ruleHandle)) {
            log.error("oauth rule handle is null, please check the rule config");
            Object error = SoulResultWarp.error(SoulResultEnum.PARAM_ERROR.getCode(), SoulResultEnum.PARAM_ERROR.getMsg(), null);
            return WebFluxResultUtils.result(exchange, error);
        }
        final OAuthHandle oauthHandle = GsonUtils.getInstance().fromJson(rule.getHandle(), OAuthHandle.class);
        if (Objects.isNull(oauthHandle.getAccessToken())) {
            log.error("accessToken is null, please check the config");
            Object error = SoulResultWarp.error(SoulResultEnum.CANNOT_FIND_ACCESSTOKEN.getCode(), SoulResultEnum.CANNOT_FIND_ACCESSTOKEN.getMsg(), null);
            return WebFluxResultUtils.result(exchange, error);
        }
        if (oauthHandle.getExipre() && oauthHandle.expired()) {
            log.error("accessToken is expire");
            Object error = SoulResultWarp.error(SoulResultEnum.ACCESSTOKEN_EXPIRED.getCode(), SoulResultEnum.ACCESSTOKEN_EXPIRED.getMsg(), null);
            return WebFluxResultUtils.result(exchange, error);
        }
        final SoulContext soulContext = exchange.getAttribute(Constants.CONTEXT);
        assert soulContext != null;
        if (!Objects.equals(soulContext.getSoulToken(), oauthHandle.getAccessToken())) {
            Object error = SoulResultWarp.error(SoulResultEnum.ACCESSTOKEN_UNAUTHED.getCode(), SoulResultEnum.ACCESSTOKEN_UNAUTHED.getMsg(), null);
            return WebFluxResultUtils.result(exchange, error);
        }
        return chain.execute(exchange);
    }

    @Override
    public String named() {
        return PluginEnum.OAUTH.getName();
    }

    @Override
    public int getOrder() {
        return PluginEnum.OAUTH.getCode();
    }

}
