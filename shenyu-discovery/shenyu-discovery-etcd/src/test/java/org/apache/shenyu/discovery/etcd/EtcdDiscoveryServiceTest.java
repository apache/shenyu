package org.apache.shenyu.discovery.etcd;

import org.apache.shenyu.discovery.api.config.DiscoveryConfig;
import org.apache.shenyu.discovery.api.listener.DataChangedEventListener;
import org.apache.shenyu.discovery.api.listener.DiscoveryDataChangedEvent;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;



class TestEventListener implements DataChangedEventListener{

    @Override
    public void onChange(DiscoveryDataChangedEvent event) {
        System.out.println(event.getKey());
        System.out.println(event.getValue());
        System.out.println(event.getEvent());

    }
}
class EtcdDiscoveryServiceTest {

    private EtcdDiscoveryService etcdDiscoveryService;

    @BeforeEach
    void setUp(){
        etcdDiscoveryService = new EtcdDiscoveryService();
        testInitWithValidConfig();
    }



    void testInitWithValidConfig() {

        DiscoveryConfig config = new DiscoveryConfig();
        config.setName("test");
        config.setType("test");
        config.setServerList("http://127.0.0.1:2379");

        this.etcdDiscoveryService.init(config);

    }

    @Test
    void watch() {
        TestEventListener tlistener = new TestEventListener();
        this.etcdDiscoveryService.watch("/shenyu/discovery", tlistener);
    }

    @Test
    void unwatch() {
    }

    @Test
    void register() {
        this.etcdDiscoveryService.register("/shenyu/discovery/service3", "666");
    }

    @Test
    void getRegisterData() {
        this.etcdDiscoveryService.register("/shenyu/discovery/service3", "666");
        System.out.println(this.etcdDiscoveryService.getRegisterData("/shenyu/discovery"));
    }

    @Test
    void exists() {
        System.out.println(this.etcdDiscoveryService.exists("greeting"));
    }

    @Test
    void shutdown() {
        this.etcdDiscoveryService.shutdown();
    }
}