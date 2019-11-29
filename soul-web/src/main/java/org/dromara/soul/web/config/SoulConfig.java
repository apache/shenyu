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

package org.dromara.soul.web.config;

import lombok.Data;
import org.dromara.soul.configuration.zookeeper.ZookeeperConfig;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

import java.io.Serializable;

/**
 * The type Soul config.
 *
 * @author xiaoyu(Myth)
 */
@Data
@ConfigurationProperties(prefix = "soul", ignoreInvalidFields = true)
public class SoulConfig implements Serializable {

    private Integer filterTime = 10;

    private Boolean filterTimeEnable = false;

    private Integer upstreamScheduledTime = 30;

    private Sync sync;

    private Disruptor disruptor;


    /**
     * The type Sync.
     */
    @Data
    public static class Sync {

        private String strategy;

        private ZookeeperConfig zookeeper;

        private HttpConfig http;

        private WebsocketConfig websocket;

    }

    @Data
    public static class HttpConfig {

        private String url;

        private Integer delayTime;

        private Integer connectionTimeout;

    }

    @Data
    public static class WebsocketConfig {

        private String url;
    }

    @Data
    public static class Disruptor {

        private Integer bufferSize;

        private Integer threadSize;

    }


}
