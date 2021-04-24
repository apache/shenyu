/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.dromara.soul.plugin.motan.proxy;

import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import com.weibo.api.motan.config.ProtocolConfig;
import com.weibo.api.motan.config.RefererConfig;
import com.weibo.api.motan.config.RegistryConfig;
import com.weibo.api.motan.proxy.CommonHandler;
import com.weibo.api.motan.rpc.ResponseFuture;
import com.weibo.breeze.message.GenericMessage;
import com.weibo.breeze.serializer.CommonSerializer;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;
import org.apache.commons.lang3.tuple.Pair;
import org.dromara.soul.common.constant.Constants;
import org.dromara.soul.common.dto.MetaData;
import org.dromara.soul.common.enums.ResultEnum;
import org.dromara.soul.common.exception.SoulException;
import org.dromara.soul.common.utils.GsonUtils;
import org.dromara.soul.plugin.api.utils.BodyParamUtils;
import org.dromara.soul.plugin.motan.cache.ApplicationConfigCache;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.Objects;
import java.util.concurrent.CompletableFuture;

/**
 * Motan proxy service.
 *
 * @author tydhot
 */
public class MotanProxyService {

    public Mono<Object> genericInvoker(final String body, final MetaData metaData, final ServerWebExchange exchange) throws SoulException {
        RefererConfig<CommonHandler> reference = ApplicationConfigCache.getInstance().get(metaData.getPath());
        if (Objects.isNull(reference) || StringUtils.isEmpty(reference.getServiceInterface())) {
            ApplicationConfigCache.getInstance().invalidate(metaData.getPath());
            reference = ApplicationConfigCache.getInstance().initRef(metaData);
        }
        CommonHandler commonHandler = reference.getRef();
        try {
            Pair<String[], Object[]> param = BodyParamUtils.buildParameters(body, metaData.getParameterTypes());
            ResponseFuture responseFuture = (ResponseFuture) commonHandler.asyncCall(metaData.getMethodName(),
                    param.getValue(), metaData.getRpcExt().getClass());
            CompletableFuture<Object> future = CompletableFuture.supplyAsync(() -> responseFuture.getValue());
            return Mono.fromFuture(future.thenApply(ret -> {
                if (Objects.isNull(ret)) {
                    ret = Constants.MOTAN_RPC_RESULT_EMPTY;
                }

                GenericMessage genericMessage = (GenericMessage) ret;
                exchange.getAttributes().put(Constants.MOTAN_RPC_RESULT, genericMessage);
                exchange.getAttributes().put(Constants.CLIENT_RESPONSE_RESULT_TYPE, ResultEnum.SUCCESS.getName());
                return ret;
            })).onErrorMap(SoulException::new);
        } catch (Throwable t) {
            throw new SoulException(t.getMessage());
        }
    }

//    private GenericMessage buildGenericMessage(String name, Map<String, Object> map) {
//        GenericMessage message = new GenericMessage();
//        message.setName(name);
//        for (Map.Entry<String, Object> e : map.entrySet()) {
//            int nameIndex = CommonSerializer.getHash(e.getKey());
//            message.putFields(nameIndex,e.getValue());
//        }
//        return message;
//    }

    public static void main(String[] args) throws Throwable {
        RegistryConfig registryConfig = new RegistryConfig();
        registryConfig.setId("soul_motan_proxy");
        registryConfig.setRegister(false);
        registryConfig.setRegProtocol("zookeeper");
        registryConfig.setAddress("localhost:2181");

        ProtocolConfig protocolConfig = new ProtocolConfig();
        protocolConfig.setId("motan2-breeze");
        protocolConfig.setName("motan2");

        RefererConfig<CommonHandler> reference = new RefererConfig<>();
        reference.setInterface(CommonHandler.class);
        reference.setServiceInterface("org.dromara.soul.examples.motan.service.MotanDemoService");
        reference.setGroup("motan-soul-rpc");
        reference.setVersion("1.0");
        reference.setRequestTimeout(1000);
        reference.setRegistry(registryConfig);
        reference.setProtocol(protocolConfig);
        CommonHandler obj = reference.getRef();

        ResponseFuture responseFuture = (ResponseFuture) obj.asyncCall("hello", new Object[]{"a"}, Object.class);
        CompletableFuture<Object> future = CompletableFuture.supplyAsync(() -> responseFuture.getValue())
                .thenApply(ret -> {
                    System.out.println(ret);
                    return ret;
                });
        future.whenComplete((aa,bb) -> {
            System.out.println(aa);
        });
        System.out.println();
        Thread.sleep(5000L);
    }
}
