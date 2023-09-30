package org.apache.shenyu.discovery.etcd;

import org.apache.shenyu.discovery.api.config.DiscoveryConfig;
import org.apache.shenyu.discovery.api.listener.DataChangedEventListener;
import org.apache.shenyu.discovery.api.listener.DiscoveryDataChangedEvent;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

public class NightTest {
    private EtcdDiscoveryService etcdDiscoveryService;

    private static final String KEY1 = "/foo/key1";

    private static final String KEY2 = "/foo/key2";

    private static final String VALUE = "value";

    private static final String GET_KEY = "getKey";

    private static final String PREFIX = "/foo";

    private static final String SEPARATOR = "/";

    private static final String EXPECTED1 = "key1";

    private static final String EXPECTED2 = "key2";

    private static final String WATCH_DATA_CHANGE_KEY = "watchDataChange";

    private static final String WATCH_CHILD_CHANGE_KEY = "WatchChildChange";


    @BeforeEach
    void setUp() throws Exception {
        // 在每个测试方法运行前，创建 Etcd 服务器和 EtcdDiscoveryService 实例
        this.etcdDiscoveryService = new EtcdDiscoveryService();
        DiscoveryConfig config = new DiscoveryConfig();
        config.setName("test");
        config.setType("test");
        config.setServerList("http://127.0.0.1:2379");

        // 调用初始化方法
        etcdDiscoveryService.init(config);
    }


    @Test
    void testWatch() {
        // 创建一个测试用的 DataChangedEventListener
        DataChangedEventListener listener = new DataChangedEventListener() {
            @Override
            public void onChange(DiscoveryDataChangedEvent event) {
                // 在监听到事件时进行断言或输出，以验证监听是否有效
                System.out.println("Received event: " + event.getKey() + ", " + event.getValue() + ", " + event.getEvent());

                // 进行断言或其他验证操作
                switch (event.getEvent()) {
                    case UPDATED:
                        Assertions.assertEquals("/shenyu/discovery/test", event.getKey());
                        Assertions.assertEquals("test-value-updated", event.getValue());
                        break;
                    case DELETED:
                        Assertions.assertEquals("/shenyu/discovery/test", event.getKey());
                        Assertions.assertNull(event.getValue());
                        break;
                    default:
                        // 忽略其他事件
                }
            }
        };

        // 调用 EtcdDiscoveryService 的 watch 方法
        etcdDiscoveryService.watch("/shenyu/discovery", listener);


        // 在 Etcd 中更新键值对
        etcdDiscoveryService.register("/shenyu/discovery/test", "test-value-updated");

        // 等待一段时间以确保监听器有机会处理事件


        // 在 Etcd 中删除键值对
        //etcdTestServer.deleteKey("/shenyu/discovery/test");

        // 等待一段时间以确保监听器有机会处理事件
        // 例如：Thread.sleep(2000);
    }
}
