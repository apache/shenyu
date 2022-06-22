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

package org.apache.shenyu.plugin.springcloud.loadbalance.client;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.dto.convert.selector.SpringCloudSelectorHandle;
import org.apache.shenyu.loadbalancer.cache.UpstreamCacheManager;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.loadbalancer.factory.LoadBalancerFactory;
import org.apache.shenyu.plugin.springcloud.handler.SpringCloudPluginDataHandler;
import org.apache.shenyu.plugin.springcloud.loadbalance.LoadBalanceKey;
import org.apache.shenyu.plugin.springcloud.loadbalance.LoadBalanceKeyHolder;
import org.springframework.cloud.client.ServiceInstance;
import org.springframework.cloud.client.discovery.DiscoveryClient;
import org.springframework.cloud.client.loadbalancer.CompletionContext;
import org.springframework.cloud.client.loadbalancer.DefaultRequest;
import org.springframework.cloud.client.loadbalancer.DefaultRequestContext;
import org.springframework.cloud.client.loadbalancer.DefaultResponse;
import org.springframework.cloud.client.loadbalancer.EmptyResponse;
import org.springframework.cloud.client.loadbalancer.HttpRequestLoadBalancerRequest;
import org.springframework.cloud.client.loadbalancer.LoadBalancerClient;
import org.springframework.cloud.client.loadbalancer.LoadBalancerLifecycle;
import org.springframework.cloud.client.loadbalancer.LoadBalancerLifecycleValidator;
import org.springframework.cloud.client.loadbalancer.LoadBalancerProperties;
import org.springframework.cloud.client.loadbalancer.LoadBalancerRequest;
import org.springframework.cloud.client.loadbalancer.LoadBalancerRequestAdapter;
import org.springframework.cloud.client.loadbalancer.LoadBalancerUriTools;
import org.springframework.cloud.client.loadbalancer.Request;
import org.springframework.cloud.client.loadbalancer.RequestData;
import org.springframework.cloud.client.loadbalancer.RequestDataContext;
import org.springframework.cloud.client.loadbalancer.ResponseData;
import org.springframework.cloud.client.loadbalancer.TimedRequestContext;
import org.springframework.cloud.client.loadbalancer.reactive.ReactiveLoadBalancer;
import org.springframework.http.HttpRequest;
import org.springframework.http.client.ClientHttpResponse;
import org.springframework.util.ReflectionUtils;

import java.io.IOException;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import static org.springframework.cloud.client.loadbalancer.reactive.ReactiveLoadBalancer.REQUEST;

/**
 * spring cloud plugin loadbalancer.
 */
public class ShenyuSpringCloudLoadBalancerClient implements LoadBalancerClient {

    private final DiscoveryClient discoveryClient;

    private final ReactiveLoadBalancer.Factory<ServiceInstance> loadBalancerClientFactory;

    public ShenyuSpringCloudLoadBalancerClient(final DiscoveryClient discoveryClient,
                                               final ReactiveLoadBalancer.Factory<ServiceInstance> loadBalancerClientFactory) {
        this.discoveryClient = discoveryClient;
        this.loadBalancerClientFactory = loadBalancerClientFactory;
    }

    @Override
    public <T> T execute(final String serviceId, final LoadBalancerRequest<T> request) throws IOException {
        String hint = getHint(serviceId);
        LoadBalancerRequestAdapter<T, TimedRequestContext> lbRequest = new LoadBalancerRequestAdapter<>(request,
                buildRequestContext(request, hint));
        Set<LoadBalancerLifecycle> supportedLifecycleProcessors = getSupportedLifecycleProcessors(serviceId);
        supportedLifecycleProcessors.forEach(lifecycle -> lifecycle.onStart(lbRequest));
        ServiceInstance serviceInstance = choose(serviceId, lbRequest);
        if (serviceInstance == null) {
            supportedLifecycleProcessors.forEach(lifecycle -> lifecycle.onComplete(
                    new CompletionContext<>(CompletionContext.Status.DISCARD, lbRequest, new EmptyResponse())));
            throw new IllegalStateException("No instances available for " + serviceId);
        }
        return execute(serviceId, serviceInstance, lbRequest);
    }

    private <T> TimedRequestContext buildRequestContext(final LoadBalancerRequest<T> delegate, final String hint) {
        if (delegate instanceof HttpRequestLoadBalancerRequest) {
            HttpRequest request = ((HttpRequestLoadBalancerRequest<?>) delegate).getHttpRequest();
            if (request != null) {
                RequestData requestData = new RequestData(request);
                return new RequestDataContext(requestData, hint);
            }
        }
        return new DefaultRequestContext(delegate, hint);
    }

    private Set<LoadBalancerLifecycle> getSupportedLifecycleProcessors(final String serviceId) {
        return LoadBalancerLifecycleValidator.getSupportedLifecycleProcessors(
                loadBalancerClientFactory.getInstances(serviceId, LoadBalancerLifecycle.class),
                DefaultRequestContext.class, Object.class, ServiceInstance.class);
    }

    @Override
    public <T> T execute(final String serviceId, final ServiceInstance serviceInstance, final LoadBalancerRequest<T> request)
            throws IOException {
        DefaultResponse defaultResponse = new DefaultResponse(serviceInstance);
        Set<LoadBalancerLifecycle> supportedLifecycleProcessors = getSupportedLifecycleProcessors(serviceId);
        Request lbRequest = request instanceof Request ? (Request) request : new DefaultRequest<>();
        supportedLifecycleProcessors
                .forEach(lifecycle -> lifecycle.onStartRequest(lbRequest, new DefaultResponse(serviceInstance)));
        try {
            T response = request.apply(serviceInstance);
            LoadBalancerProperties properties = loadBalancerClientFactory.getProperties(serviceId);
            Object clientResponse = getClientResponse(response, properties.isUseRawStatusCodeInResponseData());
            supportedLifecycleProcessors
                    .forEach(lifecycle -> lifecycle.onComplete(new CompletionContext<>(CompletionContext.Status.SUCCESS,
                            lbRequest, defaultResponse, clientResponse)));
            return response;
        }
        catch (IOException ioException) {
            supportedLifecycleProcessors.forEach(lifecycle -> lifecycle.onComplete(
                    new CompletionContext<>(CompletionContext.Status.FAILED, ioException, lbRequest, defaultResponse)));
            throw ioException;
        }
        catch (Exception exception) {
            supportedLifecycleProcessors.forEach(lifecycle -> lifecycle.onComplete(
                    new CompletionContext<>(CompletionContext.Status.FAILED, exception, lbRequest, defaultResponse)));
            ReflectionUtils.rethrowRuntimeException(exception);
        }
        return null;
    }

    private <T> Object getClientResponse(T response, boolean useRawStatusCodes) {
        ClientHttpResponse clientHttpResponse = null;
        if (response instanceof ClientHttpResponse) {
            clientHttpResponse = (ClientHttpResponse) response;
        }
        if (clientHttpResponse != null) {
            try {
                if (useRawStatusCodes) {
                    return new ResponseData(null, clientHttpResponse);
                }
                return new ResponseData(clientHttpResponse, null);
            }
            catch (IOException ignored) {
            } finally {
                clientHttpResponse.close();
            }
        }
        return response;
    }

    @Override
    public URI reconstructURI(final ServiceInstance instance, final URI original) {
        return LoadBalancerUriTools.reconstructURI(instance, original);
    }

    @Override
    public ServiceInstance choose(final String serviceId) {
        return choose(serviceId, REQUEST);
    }

    @Override
    public <T> ServiceInstance choose(final String serviceId, final Request<T> request) {
        final LoadBalanceKey loadBalanceKey = LoadBalanceKeyHolder.getLoadBalanceKey();
        List<ServiceInstance> available = this.getServiceInstance(serviceId);
        if (CollectionUtils.isEmpty(available)) {
            return null;
        }
        final SpringCloudSelectorHandle springCloudSelectorHandle = SpringCloudPluginDataHandler.SELECTOR_CACHED.get().obtainHandle(loadBalanceKey.getSelectorId());
        // not gray flow
        if (!springCloudSelectorHandle.getGray()) {
            return this.doSelect(serviceId);
        }
        List<Upstream> divideUpstreams = UpstreamCacheManager.getInstance().findUpstreamListBySelectorId(loadBalanceKey.getSelectorId());
        // gray flow,but upstream is null
        if (CollectionUtils.isEmpty(divideUpstreams)) {
            return this.doSelect(serviceId);
        }
        //select server from available to choose
        final List<Upstream> choose = new ArrayList<>(available.size());
        for (ServiceInstance serviceInstance : available) {
            divideUpstreams.stream()
                    .filter(Upstream::isStatus)
                    .filter(upstream -> Objects.equals(buildUrl(upstream), String.valueOf(serviceInstance.getUri())))
                    .findFirst().ifPresent(choose::add);
        }
        if (CollectionUtils.isEmpty(choose)) {
            return this.doSelect(serviceId);
        }
        // select by divideUpstreams
        return this.doSelect(serviceId, choose);
    }

    private static String buildUrl(final Upstream upstream) {
        if (Objects.isNull(upstream)) {
            return null;
        }
        String protocol = upstream.getProtocol();
        if (StringUtils.isBlank(protocol)) {
            protocol = "http://";
        }
        return protocol + upstream.getUrl().trim();
    }

    /**
     * select serviceInstance by shenyu loadbalancer.
     *
     * @param serviceId
     * @return
     */
    private ServiceInstance doSelect(final String serviceId) {
        List<Upstream> choose = this.buildUpstream(serviceId);
        return this.doSelect(serviceId, choose);
    }

    /**
     * execute loadbalancer by shenyu loadbalancer.
     *
     * @param serviceId serviceId
     * @param upstreamList upstream list
     * @return
     */
    private ServiceInstance doSelect(final String serviceId, final List<Upstream> upstreamList) {
        final LoadBalanceKey loadBalanceKey = LoadBalanceKeyHolder.getLoadBalanceKey();
        // default loadbalancer
        if (Objects.isNull(loadBalanceKey) || StringUtils.isEmpty(loadBalanceKey.getLoadBalance())) {
            loadBalanceKey.setLoadBalance("random");
        }
        Upstream upstream = LoadBalancerFactory.selector(upstreamList, loadBalanceKey.getLoadBalance(), loadBalanceKey.getIp());
        List<ServiceInstance> instances = this.getServiceInstance(serviceId);

        Optional<ServiceInstance> serviceInstance = instances.stream().filter(x -> {
            String url = String.valueOf(x.getUri());
            return url.equals(buildUrl(upstream));
        }).findFirst();
        if (serviceInstance.isPresent()) {
            ServiceInstance instance = serviceInstance.get();
            DefaultResponse defaultResponse = new DefaultResponse(instance);
            return defaultResponse.getServer();
        }
        return null;
    }

    /**
     * get service instance by serviceId.
     *
     * @param serviceId serviceId
     * @return {@linkplain ServiceInstance}
     */
    private List<ServiceInstance> getServiceInstance(final String serviceId) {
        List<String> serviceNames = discoveryClient.getServices().stream().map(String::toUpperCase).collect(Collectors.toList());
        if (!serviceNames.contains(serviceId.toUpperCase())) {
            return Collections.emptyList();
        }
        return discoveryClient.getInstances(serviceId);
    }

    /**
     * build upstream by service instance.
     *
     * @param serviceId serviceId
     * @return Upstream
     */
    private List<Upstream> buildUpstream(final String serviceId) {
        List<ServiceInstance> serviceInstanceList = this.getServiceInstance(serviceId);
        if (serviceInstanceList.isEmpty()) {
            return Collections.emptyList();
        }
        return serviceInstanceList.stream().map(x -> {
            String uri = x.getUri().toString();
            String[] urlPart = uri.split("\\:\\//");
            String protocol = urlPart[0];
            String url = urlPart[1];
            return Upstream.builder()
                    .protocol(protocol + "://")
                    .url(url)
                    .build();
        }).collect(Collectors.toList());
    }

    private String getHint(final String serviceId) {
        LoadBalancerProperties properties = loadBalancerClientFactory.getProperties(serviceId);
        String defaultHint = properties.getHint().getOrDefault("default", "default");
        String hintPropertyValue = properties.getHint().get(serviceId);
        return hintPropertyValue != null ? hintPropertyValue : defaultHint;
    }
}
