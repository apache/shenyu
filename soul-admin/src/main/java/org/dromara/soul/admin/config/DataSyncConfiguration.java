package org.dromara.soul.admin.config;

import org.I0Itec.zkclient.ZkClient;
import org.dromara.soul.admin.listener.DataChangedListener;
import org.dromara.soul.admin.listener.http.HttpLongPollingDataChangedListener;
import org.dromara.soul.admin.listener.websocket.WebsocketDataChangedListener;
import org.dromara.soul.admin.listener.zookeeper.ZookeeperDataChangedListener;
import org.dromara.soul.admin.mapper.PluginMapper;
import org.dromara.soul.admin.mapper.RuleConditionMapper;
import org.dromara.soul.admin.mapper.RuleMapper;
import org.dromara.soul.admin.mapper.SelectorConditionMapper;
import org.dromara.soul.admin.mapper.SelectorMapper;
import org.dromara.soul.admin.service.SyncDataService;
import org.dromara.soul.admin.service.sync.HttpSyncDataServiceImpl;
import org.dromara.soul.admin.service.sync.WebsocketSyncDataServiceImpl;
import org.dromara.soul.admin.service.sync.ZkSyncDataServiceImpl;
import org.dromara.soul.configuration.zookeeper.ZookeeperConfiguration;
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
    @ConditionalOnProperty(name = "soul.sync.strategy", havingValue = "http", matchIfMissing = true)
    static class HttpLongPollingListener {

        /**
         * Config event listener http long polling data changed listener.
         *
         * @return the http long polling data changed listener
         */
        @Bean
        public HttpLongPollingDataChangedListener configEventListener() {
            return new HttpLongPollingDataChangedListener();
        }

        /**
         * Sync data service sync data service.
         *
         * @return the sync data service
         */
        @Bean
        public SyncDataService syncDataService() {
            return new HttpSyncDataServiceImpl();
        }

    }

    /**
     * The type Zookeeper listener.
     */
    @Configuration
    @ConditionalOnMissingBean(DataChangedListener.class)
    @ConditionalOnProperty(name = "soul.sync.strategy", havingValue = "zookeeper")
    @Import(ZookeeperConfiguration.class)
    static class ZookeeperListener {

        /**
         * Config event listener data changed listener.
         *
         * @param zkClient the zk client
         * @return the data changed listener
         */
        @Bean
        public DataChangedListener configEventListener(final ZkClient zkClient) {
            return new ZookeeperDataChangedListener(zkClient);
        }

        /**
         * Sync data service sync data service.
         *
         * @param zkClient                the zk client
         * @param pluginMapper            the plugin mapper
         * @param selectorMapper          the selector mapper
         * @param selectorConditionMapper the selector condition mapper
         * @param ruleMapper              the rule mapper
         * @param ruleConditionMapper     the rule condition mapper
         * @return the sync data service
         */
        @Bean
        public SyncDataService syncDataService(final ZkClient zkClient,
                                               final PluginMapper pluginMapper,
                                               final SelectorMapper selectorMapper,
                                               final SelectorConditionMapper selectorConditionMapper,
                                               final RuleMapper ruleMapper,
                                               final RuleConditionMapper ruleConditionMapper) {
            return new ZkSyncDataServiceImpl(zkClient, pluginMapper, selectorMapper,
                    selectorConditionMapper, ruleMapper, ruleConditionMapper);
        }
    }


    /**
     * The WebsocketListener.
     */
    @Configuration
    @ConditionalOnMissingBean(DataChangedListener.class)
    @ConditionalOnProperty(name = "soul.sync.strategy", havingValue = "websocket")
    static class WebsocketListener {

        /**
         * Config event listener data changed listener.
         *
         * @return the data changed listener
         */
        @Bean
        public DataChangedListener configEventListener() {
            return new WebsocketDataChangedListener();
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

        /**
         * Sync data service sync data service.
         *
         * @return the sync data service
         */
        @Bean
        public SyncDataService syncDataService() {
            return new WebsocketSyncDataServiceImpl();
        }
    }

}
