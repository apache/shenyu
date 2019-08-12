/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.dromara.config.api;

import lombok.Data;

import java.util.List;
import java.util.Map;

/**
 * LocalConfig .
 * Basic core configuration information processing.
 *
 * @author sixh
 */
@Data
public class Config {

    /**
     * 服务器的基本配置.
     */
    private Server server;

    /**
     * 数据库的配置信息.
     */
    private DataBase dataBase;

    /**
     * 使用方自定义的配置扩展信息.
     */
    private List<Consumer> consumers;
    /**
     * 配置的数据库.
     */
    @Data
    public static class DataBase {

        /**
         * The Url.
         */
        private String url;

        private String userName;

        private String password;
    }

    /**
     * The type Server.
     */
    @Data
    public static class Server {
        /**
         * 配置中心的模式.
         */
        private String mode;

        /**
         * 集群名称.
         */
        private String cluster;

        /**
         * 服务器名称.
         */
        private String serverId;

        /**
         * 服务器端口.
         */
        private String host;
    }
}
