package org.apache.shenyu.plugin.springcloud.loadbalance.client;

import org.springframework.cloud.client.ServiceInstance;
import org.springframework.cloud.client.loadbalancer.Request;
import org.springframework.cloud.client.loadbalancer.Response;
import org.springframework.cloud.client.loadbalancer.reactive.ReactiveLoadBalancer;
import org.springframework.cloud.loadbalancer.blocking.client.BlockingLoadBalancerClient;
import reactor.core.publisher.Mono;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

/**
 * blocking loadbalance client
 */
public class CustomBlockingLoadBalancerClient extends BlockingLoadBalancerClient  {
    private final ReactiveLoadBalancer.Factory<ServiceInstance> loadBalancerClientFactory;

    public CustomBlockingLoadBalancerClient(ReactiveLoadBalancer.Factory<ServiceInstance> loadBalancerClientFactory) {
        super(loadBalancerClientFactory);
        this.loadBalancerClientFactory = loadBalancerClientFactory;
    }

    @Override
    public <T> ServiceInstance choose(String serviceId, Request<T> request) {
        ReactiveLoadBalancer<ServiceInstance> loadBalancer = loadBalancerClientFactory.getInstance(serviceId);
        if (loadBalancer == null) {
            return null;
        }
        CompletableFuture<Response<ServiceInstance>> f = CompletableFuture.supplyAsync(() -> Mono.from(loadBalancer.choose(request)).block());
        Response<ServiceInstance> loadBalancerResponse = null;
        try {
            loadBalancerResponse = f.get(3, TimeUnit.SECONDS);
        } catch (InterruptedException | ExecutionException | TimeoutException e) {
            e.printStackTrace();
        }
        if (loadBalancerResponse == null) {
            return null;
        }
        return loadBalancerResponse.getServer();
    }
}