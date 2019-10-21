/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * Contributor license agreements.See the NOTICE file distributed with
 * This work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * he License.You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.dromara.soul.plugins.http.hystrix;

import com.netflix.hystrix.HystrixCommand;
import org.asynchttpclient.AsyncHttpClient;
import org.asynchttpclient.DefaultAsyncHttpClient;
import org.asynchttpclient.DefaultAsyncHttpClientConfig;
import org.dromara.plugins.api.SoulPluginChain;
import org.dromara.soul.common.dto.SoulRequest;

/**
 * @author xiaoyu
 */
public class HttpCommand extends HystrixCommand<Object> {

    private static final AsyncHttpClient asyncHttpClient =
            new DefaultAsyncHttpClient(new DefaultAsyncHttpClientConfig.Builder()
                    .setConnectTimeout(10000)
                    .setRequestTimeout(10000)
                    .build());

    private SoulRequest soulRequest;

    private SoulPluginChain chain;

    public HttpCommand(final Setter setter) {
        super(setter);
    }

    @Override
    protected Object getFallback() {
        return super.getFallback();
    }

    @Override
    protected Object run() {
        return null;
    }
}
