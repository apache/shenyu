package org.apache.shenyu.sdk.spring;

import org.springframework.cloud.client.ServiceInstance;
import org.springframework.cloud.client.discovery.DiscoveryClient;
import org.springframework.cloud.client.loadbalancer.DefaultResponse;
import org.springframework.cloud.client.loadbalancer.Request;
import org.springframework.cloud.client.loadbalancer.Response;
import org.springframework.cloud.loadbalancer.core.ReactorServiceInstanceLoadBalancer;
import reactor.core.publisher.Mono;

public class ShenyuServiceInstanceLoadBalancer implements ReactorServiceInstanceLoadBalancer {
	
	public static String SHENYU_SERVICE_ID = "shenyu-gateway";
	
	private DiscoveryClient discoveryClient;
	
	public ShenyuServiceInstanceLoadBalancer(DiscoveryClient discoveryClient) {
		this.discoveryClient = discoveryClient;
	}
	
	@Override
	public Mono<Response<ServiceInstance>> choose(Request request) {
		return Mono.just(new DefaultResponse(discoveryClient.getInstances(SHENYU_SERVICE_ID).get(0)));
	}
	
	@Override
	public Mono<Response<ServiceInstance>> choose() {
		return ReactorServiceInstanceLoadBalancer.super.choose();
	}
}
