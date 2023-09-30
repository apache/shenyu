package org.apache.shenyu.discovery.etcd;

import org.apache.shenyu.discovery.api.config.DiscoveryConfig;

public class MainTest {
    public static void main(String[] args) {
        EtcdDiscoveryService etcdDiscoveryService = new EtcdDiscoveryService();
        DiscoveryConfig config = new DiscoveryConfig();
        config.setName("test");
        config.setType("test");
        config.setServerList("http://127.0.0.1:2379");

        // 调用初始化方法
        etcdDiscoveryService.init(config);

        String key = "/shenyu/discovery";

        System.out.println(etcdDiscoveryService.exists(key));

        System.out.println(etcdDiscoveryService.getRegisterData(key));

        TestEventListener tlistener = new TestEventListener();
        etcdDiscoveryService.watch(key, tlistener);

        etcdDiscoveryService.register("/shenyu/discovery/service4", "777");

        // etcdDiscoveryService.shutdown();


    }
}
