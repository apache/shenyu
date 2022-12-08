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

package org.apache.shenyu.sdk.core.client;

import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.UriUtils;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.loadbalancer.factory.LoadBalancerFactory;
import org.apache.shenyu.register.instance.api.ShenyuInstanceRegisterRepository;
import org.apache.shenyu.register.instance.api.config.RegisterConfig;
import org.apache.shenyu.register.instance.api.entity.InstanceEntity;
import org.apache.shenyu.sdk.core.ShenyuRequest;
import org.apache.shenyu.sdk.core.ShenyuResponse;
import org.apache.shenyu.sdk.core.interceptor.ShenyuSdkRequestInterceptor;
import org.apache.shenyu.sdk.core.retry.RetryableException;
import org.apache.shenyu.sdk.core.retry.Retryer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.net.URI;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Properties;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import static java.lang.String.format;
import static java.util.concurrent.TimeUnit.SECONDS;

/**
 * The type Abstract shenyu sdk client.
 */
public abstract class AbstractShenyuSdkClient implements ShenyuSdkClient {

    private static final Logger log = LoggerFactory.getLogger(AbstractShenyuSdkClient.class);

    private Retryer retryer;

    private ShenyuInstanceRegisterRepository registerRepository;

    private List<ShenyuSdkRequestInterceptor> requestInterceptors;

    private final Map<String, List<InstanceEntity>> watcherInstanceRegisterMap = new HashMap<>();

    private RegisterConfig registerConfig;

    private String algorithm;

    private String scheme;

    /**
     * Do request shenyu response.
     *
     * @param request the request
     * @return the shenyu response
     * @throws IOException the io exception
     */
    protected abstract ShenyuResponse doRequest(ShenyuRequest request) throws IOException;

    /**
     * Init client.
     *
     * @param props the props
     */
    protected abstract void initClient(Properties props);

    @Override
    public void init(final RegisterConfig registerConfig, final List<ShenyuSdkRequestInterceptor> requestInterceptors, final ShenyuInstanceRegisterRepository instanceRegisterRepository) {
        this.registerConfig = registerConfig;
        this.registerRepository = instanceRegisterRepository;
        this.requestInterceptors = requestInterceptors;
        Properties props = registerConfig.getProps();
        Boolean retryEnable = Optional.ofNullable(props.get("retry.enable")).map(e -> Boolean.parseBoolean(e.toString())).orElse(false);
        Long period = Optional.ofNullable(props.get("retry.period")).map(l -> Long.parseLong(l.toString())).orElse(100L);
        long maxPeriod = Optional.ofNullable(props.get("retry.maxPeriod")).map(l -> Long.parseLong(l.toString())).orElse(SECONDS.toMillis(1));
        int maxAttempts = Optional.ofNullable(props.get("retry.maxAttempts")).map(l -> Integer.parseInt(l.toString())).orElse(5);
        this.algorithm = props.getProperty("algorithm", "roundRobin");
        this.scheme = props.getProperty("scheme", "http");
        this.retryer = retryEnable ? new Retryer.DefaultRetry(period, maxPeriod, maxAttempts) : Retryer.NEVER_RETRY;
        this.initClient(props);
    }

    @Override
    public ShenyuResponse execute(final ShenyuRequest request) throws IOException {
        // do request.
        Retryer retryer = this.retryer.instance();
        while (true) {
            try {
                return execute0(request);
            } catch (RetryableException e) {
                retryer.continueOrPropagate(e);
            }
        }
    }

    private ShenyuResponse execute0(final ShenyuRequest request) {
        long start = System.nanoTime();
        ShenyuResponse shenyuResponse;
        try {
            shenyuResponse = doRequest(rewriteShenYuRequest(request));
        } catch (IOException e) {
            log.warn("request fail, retry. requestUrl {} retryCount {} elapsedTime {} ex", request.getUrl(),
                    retryer.retryCount(), TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - start), e);
            throw errorExecuting(request, e);
        }
        return shenyuResponse;
    }

    private static RetryableException errorExecuting(final ShenyuRequest request, final IOException cause) {
        return new RetryableException(
                format("%s executing %s %s", cause.getMessage(), request.getHttpMethod(), request.getUrl()),
                cause,
                null,
                request);
    }

    private ShenyuRequest rewriteShenYuRequest(final ShenyuRequest request) {
        ShenyuRequest shenyuRequest = ShenyuRequest.create(loadBalancerInstances(request), request);
        if (Objects.nonNull(requestInterceptors)) {
            requestInterceptors.forEach(interceptor -> interceptor.apply(shenyuRequest));
        }
        return shenyuRequest;
    }

    /**
     * get rewrite url by request.
     *
     * @param request the request.
     * @return {@linkplain String}
     */
    private String loadBalancerInstances(final ShenyuRequest request) {
        final List<Upstream> upstreams;
        if (Objects.isNull(registerRepository)) {
            List<String> serverList = Arrays.asList(registerConfig.getServerLists().split(","));
            if (serverList.isEmpty()) {
                throw new ShenyuException("illegal param, serverLists configuration required if registerType equals local.");
            }
            upstreams = serverList.stream()
                    .map(serverAddress -> Upstream.builder().url(UriUtils.appendScheme(serverAddress, scheme)).build())
                    .collect(Collectors.toList());
        } else {
            List<InstanceEntity> instanceRegisters = watcherInstanceRegisterMap.get(request.getName());
            if (instanceRegisters == null) {
                instanceRegisters = registerRepository.selectInstancesAndWatcher(request.getName(),
                    instanceRegisterDTOs -> watcherInstanceRegisterMap.put(request.getName(), instanceRegisterDTOs));
                watcherInstanceRegisterMap.put(request.getName(), instanceRegisters);
            }
            if (ObjectUtils.isEmpty(instanceRegisters)) {
                throw new ShenyuException("Gateway address not found from registry.");
            }
            upstreams = instanceRegisters.stream()
                    .map(instanceRegister -> {
                        final String instanceUrl = String.join(Constants.COLONS, instanceRegister.getHost(), Integer.toString(instanceRegister.getPort()));
                        return Upstream.builder().url(UriUtils.appendScheme(instanceUrl, scheme)).build();
                    })
                    .collect(Collectors.toList());
        }
        // loadBalancer
        final Upstream upstream = LoadBalancerFactory.selector(upstreams, algorithm, "");
        return replaceUrl(upstream.getUrl(), request.getUrl());
    }

    private String replaceUrl(final String url, final String sourceUrl) {
        final URI uri = URI.create(sourceUrl);
        if (StringUtils.isEmpty(uri.getQuery())) {
            return url + uri.getPath();
        } else {
            return String.format("%s%s?%s", url, uri.getPath(), uri.getQuery());
        }
    }

}
