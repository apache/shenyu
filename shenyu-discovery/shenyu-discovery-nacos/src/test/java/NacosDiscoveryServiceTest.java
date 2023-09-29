import org.apache.shenyu.discovery.api.config.DiscoveryConfig;
import org.apache.shenyu.discovery.api.listener.DataChangedEventListener;
import org.apache.shenyu.discovery.api.listener.DiscoveryDataChangedEvent;
import org.apache.shenyu.discovery.nacos.NacosDiscoveryService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import static org.mockito.Mockito.*;

public class NacosDiscoveryServiceTest {

    private NacosDiscoveryService discoveryService;

    @Mock
    private DataChangedEventListener listener;

    @BeforeEach
    public void setUp() {
        // 初始化测试对象和监听器
        MockitoAnnotations.initMocks(this);
        discoveryService = new NacosDiscoveryService();
        DiscoveryConfig cfg = new DiscoveryConfig();
        discoveryService.init(cfg);
    }

    @Test
    public void testWatchWithExistingInstance() {
        // 模拟一个已存在的服务实例
        String serviceName = "exampleService";
        String instanceId = "instance1";
        String key = serviceName + "/" + instanceId;
        String initialValue = "UP";

        // 模拟注册该服务实例
        discoveryService.register(key, initialValue);

        // 注册监听器
        discoveryService.watch(key, listener);

        // 模拟服务实例状态变化
        String updatedValue = "DOWN";
        discoveryService.register(key, updatedValue);

        // 验证监听器是否接收到了事件
        verify(listener, times(1)).onChange(any(DiscoveryDataChangedEvent.class));
    }

    @Test
    public void testWatchWithNonExistingInstance() {
        // 模拟一个不存在的服务实例
        String nonExistingKey = "nonExistingService/instance2";

        // 注册监听器
        discoveryService.watch(nonExistingKey, listener);

        // 模拟服务实例状态变化（实际上应该不会变化）

        // 验证监听器不会接收到事件
        verify(listener, never()).onChange(any(DiscoveryDataChangedEvent.class));
    }
}
