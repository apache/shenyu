/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 *
 */

package org.dromara.soul.web.configuration;

import org.dromara.soul.web.cache.LocalCacheManager;
import org.dromara.soul.web.disruptor.publisher.SoulEventPublisher;
import org.dromara.soul.web.influxdb.service.InfluxDbService;
import org.dromara.soul.web.plugin.SoulPlugin;
import org.dromara.soul.web.plugin.after.MonitorPlugin;
import org.influxdb.dto.Point;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.influxdb.DefaultInfluxDBTemplate;
import org.springframework.data.influxdb.InfluxDBConnectionFactory;
import org.springframework.data.influxdb.InfluxDBProperties;
import org.springframework.data.influxdb.InfluxDBTemplate;
import org.springframework.data.influxdb.converter.PointConverter;

/**
 * InfluxDbConfiguration.
 *
 * @author xiaoyu(Myth)
 */
@Configuration
@EnableConfigurationProperties(InfluxDBProperties.class)
@ConditionalOnProperty(prefix = "spring.influxdb", name = "url")
public class InfluxDbConfiguration {

    /**
     * init  InfluxDBConnectionFactory.
     *
     * @param influxDBProperties {@linkplain InfluxDBProperties}
     * @return {@linkplain InfluxDBConnectionFactory}
     */
    @Bean
    public InfluxDBConnectionFactory connectionFactory(final InfluxDBProperties influxDBProperties) {
        return new InfluxDBConnectionFactory(influxDBProperties);
    }

    /**
     * init  InfluxDBTemplate.
     *
     * @param connectionFactory {@linkplain InfluxDBConnectionFactory}
     * @return {@linkplain InfluxDBTemplate}
     */
    @Bean
    public InfluxDBTemplate<Point> influxDBTemplate(final InfluxDBConnectionFactory connectionFactory) {
        return new InfluxDBTemplate<>(connectionFactory, new PointConverter());
    }

    /**
     * init  DefaultInfluxDBTemplate.
     *
     * @param connectionFactory {@linkplain InfluxDBConnectionFactory}
     * @return {@linkplain DefaultInfluxDBTemplate}
     */
    @Bean
    public DefaultInfluxDBTemplate defaultTemplate(final InfluxDBConnectionFactory connectionFactory) {
        return new DefaultInfluxDBTemplate(connectionFactory);
    }

    /**
     * Influx db service influx db service.
     *
     * @param influxDBTemplate the influx db template
     * @return the influx db service
     */
    @Bean
    public InfluxDbService influxDbService(InfluxDBTemplate<Point> influxDBTemplate) {
        return new InfluxDbService(influxDBTemplate);
    }


    /**
     * Soul event publisher soul event publisher.
     *
     * @param influxDbService the influx db service
     * @return the soul event publisher
     */
    @Bean
    public SoulEventPublisher soulEventPublisher(InfluxDbService influxDbService) {
        return new SoulEventPublisher(influxDbService);
    }


    /**
     * Monitor plugin soul plugin.
     *
     * @param soulEventPublisher the soul event publisher
     * @param localCacheManager  the local cache manager
     * @return the soul plugin
     */
    @Bean
    public SoulPlugin monitorPlugin(SoulEventPublisher soulEventPublisher,
                                    LocalCacheManager localCacheManager) {
        return new MonitorPlugin(soulEventPublisher, localCacheManager);
    }

}
