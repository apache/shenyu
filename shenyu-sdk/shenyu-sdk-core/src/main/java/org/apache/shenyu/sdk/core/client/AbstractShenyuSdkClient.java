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

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.UriUtils;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.loadbalancer.factory.LoadBalancerFactory;
import org.apache.shenyu.register.common.dto.InstanceRegisterDTO;
import org.apache.shenyu.register.instance.api.ShenyuInstanceRegisterRepository;
import org.apache.shenyu.sdk.core.ShenyuRequest;
import org.apache.shenyu.sdk.core.ShenyuResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.net.URI;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

import static java.lang.String.format;
import static java.util.concurrent.TimeUnit.SECONDS;

public abstract class AbstractShenyuSdkClient implements ShenyuSdkClient {

    private static final Logger log = LoggerFactory.getLogger(AbstractShenyuSdkClient.class);

    private static final String URL_REWRITE_REGEX = ":\\/\\/[a-z\\d\\.|:]+\\/";

    private final Retryer retryer;

    private final ShenyuInstanceRegisterRepository registerRepository;

    private final Map<String, List<InstanceRegisterDTO>> watcherInstanceRegisterMap = new HashMap<>();

    private final ShenyuConfig.RegisterConfig sdkConfig;

    public AbstractShenyuSdkClient(final ShenyuConfig.RegisterConfig shenyuConfig,
                                   final ShenyuInstanceRegisterRepository shenyuInstanceRegisterRepository) {
        this.sdkConfig = shenyuConfig;
        this.registerRepository = shenyuInstanceRegisterRepository;
        Boolean retryEnable = Optional.ofNullable(sdkConfig.getProps().get("retry.enable")).map(e -> (boolean) e).orElse(false);
        Long period = Optional.ofNullable(sdkConfig.getProps().get("retry.period")).map(l -> (Long) l).orElse(100L);
        long maxPeriod = Optional.ofNullable(sdkConfig.getProps().get("retry.maxPeriod")).map(l -> (Long) l).orElse(SECONDS.toMillis(1));
        int maxAttempts = Optional.ofNullable(sdkConfig.getProps().get("retry.maxAttempts")).map(l -> (int) l).orElse(5);
        this.retryer = retryEnable ? new Retryer.DefaultRetry(period, maxPeriod, maxAttempts) : Retryer.NEVER_RETRY;

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
        return ShenyuRequest.create(loadBalancerInstances(request), request);
    }

    /**
     * TODO wait append load balance logic.
     * get rewrite url by request.
     *
     * @param request the request.
     * @return {@linkplain String}
     */
    private String loadBalancerInstances(final ShenyuRequest request) {
        if (StringUtils.isEmpty(sdkConfig.getRegisterType())) {
            throw new ShenyuException("configure registerType is required.");
        }
        final List<Upstream> upstreams;
        if ("local".equals(sdkConfig.getRegisterType())) {
            List<String> serverList = Arrays.asList(sdkConfig.getServerLists().split(","));
            if (serverList.isEmpty()) {
                throw new ShenyuException("illegal param, serverLists configuration required if registerType equals local.");
            }
            upstreams = serverList.stream()
                    .map(serverAddress -> Upstream.builder().url(UriUtils.appendScheme(serverAddress, "http")).build())
                    .collect(Collectors.toList());
        } else {
            List<InstanceRegisterDTO> instanceRegisters = watcherInstanceRegisterMap.get(request.getContextId());
            if (instanceRegisters == null) {
                instanceRegisters = registerRepository.selectInstancesAndWatcher(request.getContextId(),
                    instanceRegisterDTOs -> watcherInstanceRegisterMap.put(request.getContextId(), instanceRegisterDTOs));
            }
            upstreams = instanceRegisters.stream()
                    .map(instanceRegister -> {
                        final String instanceUrl = String.join(Constants.COLONS, instanceRegister.getHost(), Integer.toString(instanceRegister.getPort()));
                        return Upstream.builder().url(UriUtils.appendScheme(instanceUrl, "http")).build();
                    })
                    .collect(Collectors.toList());
        }
        // loadBalancer
        final Upstream upstream = LoadBalancerFactory.selector(upstreams, "roundRobin", "");
        final String path = URI.create(request.getUrl()).getPath();
        return upstream.getUrl() + path;
    }

    protected abstract ShenyuResponse doRequest(ShenyuRequest request) throws IOException;

}
