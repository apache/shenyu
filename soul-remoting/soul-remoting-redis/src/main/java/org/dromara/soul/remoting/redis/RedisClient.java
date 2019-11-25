/*
 *     Licensed to the Apache Software Foundation (ASF) under one or more
 *     contributor license agreements.See the NOTICE file distributed with
 *     this work for additional information regarding copyright ownership.
 *     The ASF licenses this file to You under the Apache License, Version 2.0
 *     (the "License"); you may not use this file except in compliance with
 *     the License.You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *     Unless required by applicable law or agreed to in writing, software
 *     distributed under the License is distributed on an "AS IS" BASIS,
 *     WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *     See the License for the specific language governing permissions and
 *     limitations under the License.
 */

package org.dromara.soul.remoting.redis;

import com.google.common.base.Splitter;
import com.google.common.collect.Sets;
import java.util.HashSet;
import java.util.Set;
import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.extension.ExtensionLoader;
import org.dromara.soul.common.http.URL;
import org.dromara.soul.common.utils.AttrMap;
import org.dromara.soul.common.utils.StringUtils;
import org.dromara.soul.remoting.redis.operation.*;

/**
 * RedisClient
 *
 * @author sixh
 * @see URL reids://192.168.1.2:2181?cluster=192.168.1.3:2181,192.168.1.4:2181&client=jedis&mode=cluster&password=123456&timeOut=100000
 */
public class RedisClient<K, V> implements RedisOperation {

    private RedisOperation<K, V> operation;

    private URL url;

    private ExtensionLoader<RedisOperationFactory> loader = ExtensionLoader.getExtensionLoader(RedisOperationFactory.class);

    public RedisClient(URL url) {
        this.url = url;
        AttrMap<String, Object> attrMap = url.toAttrMap();
        RedisOperationFactory operationFactory = attrMap.getString(RemotingRedisConst.URL_CLIENT_KEY)
                .map(client -> loader.getJoin(client))
                .orElse(loader.getDefaultJoin());
        RedisModule module = new RedisModule();
        String cluster = attrMap.getString(RemotingRedisConst.URL_CLUSTER_KEY).orElse("");
        String host = url.getHost();
        int port = url.getPort();
        Set<String> hosts = new HashSet<>();
        hosts.add(host + Constants.COLONS + port);
        if (StringUtils.isNotBlank(cluster)) {
            hosts.addAll(Sets.newHashSet(Splitter.on(",").trimResults().split(cluster)));
        }
        module.setMode(attrMap.getString(RemotingRedisConst.URL_MODE_KEY).orElse(RemotingRedisConst.MODE_DEFAULT));
        module.setHosts(hosts);
        module.setMasterName(attrMap.getString(RemotingRedisConst.URL_MASTER_NAME_KEY).orElse(""));
        module.setPassword(attrMap.getString(RemotingRedisConst.URL_PASSWORD_KEY).orElse(null));
        module.setMaxTotal(attrMap.getInt(RemotingRedisConst.URL_MAX_TOTAL_KEY).orElse(RemotingRedisConst.MAX_TOTAL_DEFAULT));
        module.setMaxIdle(attrMap.getInt(RemotingRedisConst.URL_MAX_IDLE_KEY).orElse(RemotingRedisConst.MAX_IDLE_DEFAULT));
        module.setMinIdle(attrMap.getInt(RemotingRedisConst.URL_MIN_IDLE_KEY).orElse(1));
        module.setMaxWaitMillis(attrMap.getLong(RemotingRedisConst.URL_MAX_WAIT_MILLIS_KEY).orElse(RemotingRedisConst.MAX_WAIT_MILLIS_DEFAULT));
        module.setMinEvictableIdleTimeMillis(attrMap.getLong(RemotingRedisConst.URL_MIN_EVICTABLE_IDLE_TIME_MILLIS_KEY).orElse(RemotingRedisConst.MIN_EVICTABLE_IDLE_TIME_MILLIS_DEFAULT));
        module.setSoftMinEvictableIdleTimeMillis(attrMap.getLong(RemotingRedisConst.URL_SOFT_MIN_EVICTABLE_IDLE_TIME_MILLIS_KEY).orElse(RemotingRedisConst.SOFT_MIN_EVICTABLE_IDLE_TIME_MILLIS_DEFAULT));
        module.setNumTestsPerEvictionRun(attrMap.getInt(RemotingRedisConst.URL_NUM_TESTS_PER_EVICTION_RUN_KEY).orElse(RemotingRedisConst.NUM_TESTS_PER_EVICTION_RUN_DEFAULT));
        module.setTestOnCreate(attrMap.getBool(RemotingRedisConst.URL_TEST_ON_CREATE_KEY).orElse(RemotingRedisConst.TEST_ON_CREATE_DEFAULT));
        module.setTestOnBorrow(attrMap.getBool(RemotingRedisConst.URL_TEST_ON_BORROW_KEY).orElse(RemotingRedisConst.TEST_ON_BORROW_DEFAULT));
        module.setTestOnReturn(attrMap.getBool(RemotingRedisConst.URL_TEST_ON_RETURN_KEY).orElse(RemotingRedisConst.TEST_ON_RETURN_DEFAULT));
        module.setTestWhileIdle(attrMap.getBool(RemotingRedisConst.URL_TEST_WHILE_IDLE_KEY).orElse(RemotingRedisConst.TEST_WHILE_IDLE_DEFAULT));
        module.setTimeBetweenEvictionRunsMillis(attrMap.getLong(RemotingRedisConst.URL_TIME_BETWEEN_EVICTION_RUNS_MILLIS_KEY).orElse(RemotingRedisConst.TIME_BETWEEN_EVICTION_RUNS_MILLIS_DEFAULT));
        module.setBlockWhenExhausted(attrMap.getBool(RemotingRedisConst.URL_BLOCK_WHEN_EXHAUSTED_KEY).orElse(RemotingRedisConst.BLOCK_WHEN_EXHAUSTED_DEFAULT));
        module.setTimeOut(attrMap.getInt(RemotingRedisConst.URL_TIME_OUT_KEY).orElse(RemotingRedisConst.TIME_OUT_DEFAULT));
        this.operation = operationFactory.getOperation(module);
    }

    public URL getUrl() {
        return url;
    }

    @Override
    public MapOperation mapOperation() {
        return operation.mapOperation();
    }

    @Override
    public ListOperation listOperation() {
        return operation.listOperation();
    }

    @Override
    public SetOperation setOperation() {
        return operation.setOperation();
    }

    @Override
    public ValueOperation<K, V> valueOperation() {
        return operation.valueOperation();
    }

    @Override
    public ZsetOperation zsetOperation() {
        return operation.zsetOperation();
    }

    @Override
    public ScriptOperation scriptOperation() {
        return operation.scriptOperation();
    }
}
