package org.dromara.soul.bootstrap.springcloud;

import org.dromara.soul.bootstrap.BaseTest;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cloud.client.ServiceInstance;
import org.springframework.cloud.client.loadbalancer.LoadBalancerClient;

import java.net.URI;

/**
 * @author xiaoyu(549477611 @ qq.com)
 */
public class LoadBalancerClientTest extends BaseTest {

    @Autowired
    private LoadBalancerClient loadBalancerClient;

    private URI uri = URI.create("/test");

    @Test
    public void testChoose() {
        for (int i = 0; i < 10; i++) {
            final ServiceInstance serviceInstance
                    = loadBalancerClient.choose("TEST-SERVICE");
            System.out.println(serviceInstance.toString());

            final URI uri = loadBalancerClient.reconstructURI(serviceInstance, this.uri);
            System.out.println(uri);
        }
    }
}
