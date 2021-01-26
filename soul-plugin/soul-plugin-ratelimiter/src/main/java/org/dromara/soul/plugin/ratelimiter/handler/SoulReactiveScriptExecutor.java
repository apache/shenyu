package org.dromara.soul.plugin.ratelimiter.handler;

import java.nio.ByteBuffer;
import java.util.List;

import org.springframework.data.redis.connection.ReactiveRedisConnection;
import org.springframework.data.redis.connection.ReactiveRedisConnectionFactory;
import org.springframework.data.redis.connection.ReturnType;
import org.springframework.data.redis.core.ReactiveRedisCallback;
import org.springframework.data.redis.core.script.DefaultReactiveScriptExecutor;
import org.springframework.data.redis.core.script.RedisScript;
import org.springframework.data.redis.serializer.RedisElementReader;
import org.springframework.data.redis.serializer.RedisElementWriter;
import org.springframework.data.redis.serializer.RedisSerializationContext;
import org.springframework.util.Assert;

import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

/**
 * The type reactive script executor.
 *
 * @author zhanglei
 */
public class SoulReactiveScriptExecutor<K> extends DefaultReactiveScriptExecutor<K> {

    public SoulReactiveScriptExecutor(final ReactiveRedisConnectionFactory connectionFactory,
                                      final RedisSerializationContext<K, ?> serializationContext) {
        super(connectionFactory, serializationContext);
    }

    @Override
    public <T> Flux<T> execute(final RedisScript<T> script, final List<K> keys, final List<?> args, final RedisElementWriter<?> argsWriter,
                               final RedisElementReader<T> resultReader) {

        Assert.notNull(script, "RedisScript must not be null!");
        Assert.notNull(argsWriter, "Argument Writer must not be null!");
        Assert.notNull(resultReader, "Result Reader must not be null!");
        Assert.notNull(keys, "Keys must not be null!");
        Assert.notNull(args, "Args must not be null!");
        return execute(connection -> {
            ReturnType returnType = ReturnType.fromJavaType(script.getResultType());
            ByteBuffer[] keysAndArgs = keysAndArgs(argsWriter, keys, args);
            int keySize = keys.size();
            return super.eval(connection, script, returnType, keySize, keysAndArgs, resultReader);

        });
    }

    private <T> Flux<T> execute(final ReactiveRedisCallback<T> action) {
        Assert.notNull(action, "Callback object must not be null");
        ReactiveRedisConnectionFactory factory = getConnectionFactory();
        return Flux.usingWhen(Mono.fromSupplier(factory::getReactiveConnection), action::doInRedis,
                ReactiveRedisConnection::closeLater);
    }

}
