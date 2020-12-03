package org.dromara.soul.sync.data.http;

import org.dromara.soul.common.dto.AppAuthData;
import org.dromara.soul.common.dto.MetaData;
import org.dromara.soul.common.dto.PluginData;
import org.dromara.soul.sync.data.api.AuthDataSubscriber;
import org.dromara.soul.sync.data.api.MetaDataSubscriber;
import org.dromara.soul.sync.data.api.PluginDataSubscriber;
import org.dromara.soul.sync.data.http.config.HttpConfig;
import org.dromara.soul.sync.data.http.support.MockHttpDataSyncEndpoint;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

import java.util.Collections;
import java.util.List;
import java.util.concurrent.TimeUnit;


/**
 * Test cases for {@link HttpSyncDataService}
 *
 * @author davidliu
 */
public class HttpSyncDataServiceTest {
    
    // mock HttpDataSyncEndpoint at localhost 8080
    private static final MockHttpDataSyncEndpoint server = new MockHttpDataSyncEndpoint(8080);
    
    
    @BeforeClass
    public static void beforeCase() throws Exception {
        server.start();
    }
    
    @AfterClass
    public static void afterCase() throws Exception {
        server.stop();
    }
    
    /**
     * this method covers {@link HttpSyncDataService} constructor and {@link HttpSyncDataService#close()} method
     *
     * @throws Exception any exception
     */
    @Test
    public void test() throws Exception {
        try (HttpSyncDataService ignored = this.buildHttpSyncDataService()) {
            // sleep 5 seconds to ensure Http Long polling task run
            TimeUnit.SECONDS.sleep(5);
        }
        
    }
    
    private HttpSyncDataService buildHttpSyncDataService() {
        HttpConfig httpConfig = new HttpConfig();
        httpConfig.setUrl("http://localhost:8080");
        // set http connection timeout
        httpConfig.setConnectionTimeout(3);
        // set delay time
        httpConfig.setDelayTime(3);
        PluginDataSubscriber pluginDataSubscriber = new PluginDataSubscriber() {
            @Override
            public void onSubscribe(PluginData pluginData) {
            
            }
        };
        List<MetaDataSubscriber> metaDataSubscribers = Collections.singletonList(new MetaDataSubscriber() {
            @Override
            public void onSubscribe(MetaData metaData) {
            
            }
            
            @Override
            public void unSubscribe(MetaData metaData) {
            
            }
        });
        List<AuthDataSubscriber> authDataSubscribers = Collections.singletonList(new AuthDataSubscriber() {
            @Override
            public void onSubscribe(AppAuthData appAuthData) {
            
            }
            
            @Override
            public void unSubscribe(AppAuthData appAuthData) {
            
            }
        });
        return new HttpSyncDataService(httpConfig, pluginDataSubscriber, metaDataSubscribers, authDataSubscribers);
    }
}