package org.apache.shenyu.examples.plugin.wasm;

import com.google.gson.JsonObject;
import io.github.kawamuray.wasmtime.Func;
import io.github.kawamuray.wasmtime.Store;
import io.github.kawamuray.wasmtime.WasmFunctions;
import io.github.kawamuray.wasmtime.WasmValType;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.result.ShenyuResultEnum;
import org.apache.shenyu.plugin.api.utils.WebFluxResultUtils;
import org.apache.shenyu.plugin.wasm.api.AbstractWasmPlugin;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.net.URI;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

/**
 * The type RustHttpClientPlugin.
 */
public class RustHttpClientPlugin extends AbstractWasmPlugin {

    private static final Map<Long, String> PARAMS = new ConcurrentHashMap<>();

    private static final Map<Long, String> RESULTS = new ConcurrentHashMap<>();

    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain, final Long argumentId) {
        final String result = RESULTS.get(argumentId);
        return WebFluxResultUtils.result(exchange, result);
    }

    @Override
    protected Long getArgumentId(final ServerWebExchange exchange, final ShenyuPluginChain chain) {
        final UUID uuid = UUID.randomUUID();
        long argumentId = uuid.getMostSignificantBits() ^ uuid.getLeastSignificantBits();
        final String httpMethod = exchange.getRequest().getMethodValue();
        final URI uri = exchange.getAttribute(Constants.HTTP_URI);
        if (Objects.isNull(uri)) {
            throw new ShenyuException(ShenyuResultEnum.CANNOT_FIND_URL.getMsg());
        }
        // In this example, only GET requests are supported
        JsonObject json = new JsonObject();
        json.addProperty("method", httpMethod);
        json.addProperty("uri", uri.toString());
        PARAMS.put(argumentId, json.toString());
        return argumentId;
    }

    @Override
    protected Map<String, Func> initWasmCallJavaFunc(final Store<Void> store) {
        Map<String, Func> funcMap = new HashMap<>();
        funcMap.put("get_args", WasmFunctions.wrap(store, WasmValType.I64, WasmValType.I64, WasmValType.I32, WasmValType.I32,
                (argId, addr, len) -> {
                    String config = PARAMS.get(argId);
                    LOG.info("java side put->{}", config);
                    ByteBuffer buf = super.getBuffer();
                    for (int i = 0; i < len && i < config.length(); i++) {
                        buf.put(addr.intValue() + i, (byte) config.charAt(i));
                    }
                    return Math.min(config.length(), len);
                }));
        funcMap.put("put_result", WasmFunctions.wrap(store, WasmValType.I64, WasmValType.I64, WasmValType.I32,
                (argId, addr, len) -> {
                    ByteBuffer buf = super.getBuffer();
                    byte[] bytes = new byte[len];
                    for (int i = 0; i < len; i++) {
                        bytes[i] = buf.get(addr.intValue() + i);
                    }
                    String result = new String(bytes, StandardCharsets.UTF_8);
                    RESULTS.put(argId, result);
                    LOG.info("java side get->{}", result);
                }));
        return funcMap;
    }

    @Override
    public int getOrder() {
        return PluginEnum.NETTY_HTTP_CLIENT.getCode() - 1;
    }

    @Override
    public boolean skip(final ServerWebExchange exchange) {
        return skipExceptHttpLike(exchange);
    }

    @Override
    public String named() {
        return "rustHttpClient";
    }
}
