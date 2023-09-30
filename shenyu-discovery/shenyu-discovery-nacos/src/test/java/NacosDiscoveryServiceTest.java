import org.apache.shenyu.discovery.api.config.DiscoveryConfig;
import org.apache.shenyu.discovery.api.listener.DataChangedEventListener;
import org.apache.shenyu.discovery.api.listener.DiscoveryDataChangedEvent;
import org.apache.shenyu.discovery.nacos.NacosDiscoveryService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import static org.mockito.Mockito.*;

//public class NacosDiscoveryServiceTest {
//
//    private NacosDiscoveryService discoveryService;
//
//    @Mock
//    private DataChangedEventListener listener;
//
//    @BeforeEach
//    public void setUp() {
//        //   MockitoAnnotations.initMocks(this);
//        discoveryService = new NacosDiscoveryService();
//        DiscoveryConfig cfg = new DiscoveryConfig();
//        discoveryService.init(cfg);
//    }
//
//    @Test
//    public void testWatchWithExistingInstance() {
//        // mock a existed service instance
//        String serviceName = "exampleService";
//        String instanceId = "instance1";
//        String key = serviceName + "/" + instanceId;
//        String initialValue = "UP";
//
//        // mock register
//        discoveryService.register(key, initialValue);
//
//        // mock watch
//        discoveryService.watch(key, listener);
//
//        String updatedValue = "DOWN";
//        discoveryService.register(key, updatedValue);
//
//        verify(listener, times(1)).onChange(any(DiscoveryDataChangedEvent.class));
//    }
//
//    @Test
//    public void testWatchWithNonExistingInstance() {
//        String nonExistingKey = "nonExistingService/instance2";
//
//        discoveryService.watch(nonExistingKey, listener);
//
//        verify(listener, never()).onChange(any(DiscoveryDataChangedEvent.class));
//    }
//}
