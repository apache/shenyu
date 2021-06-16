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

package org.apache.shenyu.springboot.starter.plugin.response;

import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.response.ResponsePlugin;
import org.apache.shenyu.plugin.response.strategy.MessageWriter;
import org.apache.shenyu.plugin.response.strategy.NettyClientMessageWriter;
import org.apache.shenyu.plugin.response.strategy.RPCMessageWriter;
import org.apache.shenyu.plugin.response.strategy.WebClientMessageWriter;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.HashMap;
import java.util.Map;

/**
 * The type response plugin configuration.
 */
@Configuration
public class ResponsePluginConfiguration {
    
    /**
     * Response plugin shenyu plugin.
     *
     * @param httpWriter the http writer
     * @return the shenyu plugin
     */
    @Bean
    public ShenyuPlugin responsePlugin(final ObjectProvider<MessageWriter> httpWriter) {
        Map<String, MessageWriter> writerMap = new HashMap<>();
        MessageWriter httpWrite = httpWriter.getIfAvailable();
        MessageWriter rpcWrite = new RPCMessageWriter();
        writerMap.put(RpcTypeEnum.HTTP.getName(), httpWrite);
        writerMap.put(RpcTypeEnum.SPRING_CLOUD.getName(), httpWrite);
        writerMap.put(RpcTypeEnum.DUBBO.getName(), rpcWrite);
        writerMap.put(RpcTypeEnum.SOFA.getName(), rpcWrite);
        writerMap.put(RpcTypeEnum.GRPC.getName(), rpcWrite);
        writerMap.put(RpcTypeEnum.MOTAN.getName(), rpcWrite);
        writerMap.put(RpcTypeEnum.TARS.getName(), rpcWrite);
        return new ResponsePlugin(writerMap);
    }
    
    /**
     * The type Web client message writer configuration.
     */
    @Configuration
    @ConditionalOnProperty(name = "shenyu.httpclient.strategy", havingValue = "webClient", matchIfMissing = true)
    static class WebClientMessageWriterConfiguration {
    
        /**
         * Web client message writer message writer.
         *
         * @return the message writer
         */
        @Bean
        public MessageWriter webClientMessageWriter() {
            return new WebClientMessageWriter();
        }
    }
    
    /**
     * The type Netty client message writer configuration.
     */
    @Configuration
    @ConditionalOnProperty(name = "shenyu.httpclient.strategy", havingValue = "netty")
    static class NettyClientMessageWriterConfiguration {
    
        /**
         * Netty message writer message writer.
         *
         * @return the message writer
         */
        @Bean
        public MessageWriter nettyMessageWriter() {
            return new NettyClientMessageWriter();
        }
    }
}
