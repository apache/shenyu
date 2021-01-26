package org.dromara.soul.plugin.ratelimiter.handler;

import java.util.List;

import org.springframework.data.redis.connection.ReactiveRedisConnectionFactory;
import org.springframework.data.redis.core.ReactiveRedisTemplate;
import org.springframework.data.redis.core.script.ReactiveScriptExecutor;
import org.springframework.data.redis.core.script.RedisScript;
import org.springframework.data.redis.serializer.RedisElementReader;
import org.springframework.data.redis.serializer.RedisElementWriter;
import org.springframework.data.redis.serializer.RedisSerializationContext;

import reactor.core.publisher.Flux;

/**
 * The type reactive redisTemplate.
 *
 * @author zhanglei
 */
public class SoulReactiveRedisTemplate<K, V> extends ReactiveRedisTemplate<K, V> {

    private final ReactiveScriptExecutor<K> reactiveScriptExecutor;

    public SoulReactiveRedisTemplate(final ReactiveRedisConnectionFactory connectionFactory,
                                     final RedisSerializationContext<K, V> serializationContext) {

        super(connectionFactory, serializationContext);
        this.reactiveScriptExecutor = new SoulReactiveScriptExecutor<>(connectionFactory, serializationContext);
    }

    @Override
    public <T> Flux<T> execute(final RedisScript<T> script, final List<K> keys, final List<?> args) {
        return reactiveScriptExecutor.execute(script, keys, args);
    }

    @Override
    public <T> Flux<T> execute(final RedisScript<T> script, final List<K> keys, final List<?> args, final RedisElementWriter<?> argsWriter,
                               final RedisElementReader<T> resultReader) {
        return reactiveScriptExecutor.execute(script, keys, args, argsWriter, resultReader);
    }

}
