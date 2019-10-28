/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.You may obtain a copy of the License at
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.dromara.soul.plugins.http;

import org.asynchttpclient.AsyncHttpClient;
import org.asynchttpclient.BoundRequestBuilder;
import org.asynchttpclient.DefaultAsyncHttpClient;
import org.asynchttpclient.DefaultAsyncHttpClientConfig;
import org.asynchttpclient.Response;
import org.dromara.plugins.api.AbstractSoulPlugin;
import org.dromara.plugins.api.SoulPluginChain;
import org.dromara.plugins.api.dto.SoulRequest;
import org.dromara.plugins.api.dto.SoulResponse;
import org.dromara.soul.cache.api.data.SelectorData;
import org.dromara.soul.common.dto.convert.DivideUpstream;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.common.enums.PluginTypeEnum;
import org.dromara.soul.common.extension.ExtensionLoader;
import org.dromara.soul.common.http.HttpMethod;
import org.dromara.soul.common.utils.GsonUtils;
import org.dromara.soul.fusing.api.FusingService;
import org.dromara.soul.fusing.api.config.FusingConfig;
import org.dromara.soul.plugins.http.balance.LoadBalance;

import java.util.concurrent.CompletableFuture;

/**
 * The type Http plugin.
 *
 * @author xiaoyu(Myth)
 */
public class HttpPlugin extends AbstractSoulPlugin {

    private static final AsyncHttpClient asyncHttpClient =
            new DefaultAsyncHttpClient(new DefaultAsyncHttpClientConfig.Builder()
                    .setConnectTimeout(3000)
                    .setRequestTimeout(3000)
                    .build());

    @Override
    public PluginTypeEnum pluginType() {
        return PluginTypeEnum.FUNCTION;
    }

    @Override
    public int getOrder() {
        return PluginEnum.HTTP.getOrder();
    }

    @Override
    public String named() {
        return PluginEnum.HTTP.getName();
    }

    @Override
    public Boolean skip(SoulRequest soulRequest) {
        return false;
    }

    @Override
    protected SoulResponse doExecute(SoulRequest soulRequest, SelectorData selectorData, SoulPluginChain chain) {

        String handle = selectorData.getHandle();

        final FusingConfig fusingConfig = GsonUtils.getInstance().fromJson(handle, FusingConfig.class);

        LoadBalance loadBalance = ExtensionLoader.getExtensionLoader(LoadBalance.class).getDefaultJoin();

        DivideUpstream divideUpstream = loadBalance.select(null, "");

        FusingService fusingService = ExtensionLoader.getExtensionLoader(FusingService.class).getDefaultJoin();

        Object result = fusingService.execute(fusingConfig, () -> {
            BoundRequestBuilder boundRequestBuilder;
            if (soulRequest.getHttpMethod() == HttpMethod.GET) {
                boundRequestBuilder = asyncHttpClient.prepareGet(soulRequest.getUrl());
            } else if (soulRequest.getHttpMethod() == HttpMethod.POST) {
                boundRequestBuilder = asyncHttpClient.preparePost(soulRequest.getUrl());
                boundRequestBuilder.setBody(soulRequest.getBody());
            } else {
                boundRequestBuilder = asyncHttpClient.preparePut(soulRequest.getUrl());
            }
            CompletableFuture<Response> whenResponse = boundRequestBuilder
                    .setSingleHeaders(soulRequest.getHeaders())
                    .execute()
                    .toCompletableFuture()
                    .thenApply(response -> response);
            return whenResponse.join().getResponseBody();
        }, throwable -> {
            return null;
        });

        return chain.execute(soulRequest);
    }
}
