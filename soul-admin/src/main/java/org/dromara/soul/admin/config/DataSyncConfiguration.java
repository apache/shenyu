package org.dromara.soul.admin.config;

import com.alibaba.nacos.api.config.ConfigService;
import org.I0Itec.zkclient.ZkClient;
import org.dromara.soul.admin.listener.DataChangedListener;
import org.dromara.soul.admin.listener.http.HttpLongPollingDataChangedListener;
import org.dromara.soul.admin.listener.nacos.NacosDataChangedListener;
import org.dromara.soul.admin.listener.websocket.WebsocketCollector;
import org.dromara.soul.admin.listener.websocket.WebsocketDataChangedListener;
import org.dromara.soul.admin.listener.zookeeper.ZookeeperDataChangedListener;
import org.dromara.soul.admin.listener.zookeeper.ZookeeperDataInit;
import org.dromara.soul.admin.service.SyncDataService;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.web.socket.server.standard.ServerEndpointExporter;

/**
 * The type Data sync configuration.
 *
 * @author xiaoyu
 * @author huangxiaofeng
 */
@Configuration
public class DataSyncConfiguration {
    
    /**
     * http long polling(default strategy).
     */
    @Configuration
    @ConditionalOnMissingBean(DataChangedListener.class)
    @ConditionalOnProperty(name = "soul.sync.http")
    @Import(HttpLongPollingDataChangedListener.class)
    static class HttpLongPollingListener {
    }
    
    /**
     * The type Zookeeper listener.
     */
    @Configuration
    @ConditionalOnMissingBean(DataChangedListener.class)
    @ConditionalOnProperty(prefix = "soul.sync.zookeeper", name = "url")
    @Import(ZookeeperConfiguration.class)
    static class ZookeeperListener {
    
        /**
         * Config event listener data changed listener.
         *
         * @param zkClient the zk client
         * @return the data changed listener
         */
        @Bean
        public DataChangedListener dataChangedListener(final ZkClient zkClient) {
            return new ZookeeperDataChangedListener(zkClient);
        }
    
        /**
         * Zookeeper data init zookeeper data init.
         *
         * @param zkClient        the zk client
         * @param syncDataService the sync data service
         * @return the zookeeper data init
         */
        @Bean
        public ZookeeperDataInit zookeeperDataInit(final ZkClient zkClient, final SyncDataService syncDataService) {
            return new ZookeeperDataInit(zkClient, syncDataService);
        }
    }
    
    /**
     * The type Nacos listener.
     */
    @Configuration
    @ConditionalOnMissingBean(DataChangedListener.class)
    @ConditionalOnProperty(prefix = "soul.sync.nacos", name = "url")
    @Import(NacosConfiguration.class)
    static class NacosListener {
    
        /**
         * Data changed listener data changed listener.
         *
         * @param configService the config service
         * @return the data changed listener
         */
        @Bean
        public DataChangedListener dataChangedListener(final ConfigService configService) {
            return new NacosDataChangedListener(configService);
        }
    }
    
    /**
     * The WebsocketListener.
     */
    @Configuration
    @ConditionalOnMissingBean(DataChangedListener.class)
    @ConditionalOnProperty(name = "soul.sync.websocket")
    static class WebsocketListener {
    
        /**
         * Config event listener data changed listener.
         *
         * @return the data changed listener
         */
        @Bean
        public DataChangedListener dataChangedListener() {
            return new WebsocketDataChangedListener();
        }
    
        /**
         * Websocket collector websocket collector.
         *
         * @return the websocket collector
         */
        @Bean
        public WebsocketCollector websocketCollector() {
            return new WebsocketCollector();
        }
    
        /**
         * Server endpoint exporter server endpoint exporter.
         *
         * @return the server endpoint exporter
         */
        @Bean
        public ServerEndpointExporter serverEndpointExporter() {
            return new ServerEndpointExporter();
        }
    }
}

