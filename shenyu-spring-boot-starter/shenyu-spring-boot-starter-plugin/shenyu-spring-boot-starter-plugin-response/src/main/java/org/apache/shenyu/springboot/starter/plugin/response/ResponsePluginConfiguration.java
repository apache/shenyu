/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.shenyu.springboot.starter.plugin.response;

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

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * The type response plugin configuration.
 */
@Configuration
@ConditionalOnProperty(value = {"shenyu.plugins.response.enabled"}, havingValue = "true", matchIfMissing = true)
public class ResponsePluginConfiguration {
    
    /**
     * Response plugin.
     *
     * @param httpWriter the http writer
     * @return the shenyu plugin
     */
    @Bean
    public ShenyuPlugin responsePlugin(final ObjectProvider<List<MessageWriter>> httpWriter) {
        Map<String, MessageWriter> writerMap = new LinkedHashMap<>();
        List<MessageWriter> writerList = httpWriter.getIfAvailable(ArrayList::new);
        for (MessageWriter writer : writerList) {
            List<String> supportTypes = writer.supportTypes();
            for (String type : supportTypes) {
                writerMap.put(type, writer);
            }
        }
        return new ResponsePlugin(writerMap);
    }
    
    /**
     * Rpc message writer message writer.
     *
     * @return the message writer
     */
    @Bean
    @ConditionalOnProperty(name = "shenyu.plugins.response.rpc-message-writer", havingValue = "true", matchIfMissing = true)
    public MessageWriter rpcMessageWriter() {
        return new RPCMessageWriter();
    }
    
    /**
     * The type Web client message writer configuration.
     */
    @Configuration
    @ConditionalOnProperty(name = "shenyu.httpclient.strategy", havingValue = "webClient", matchIfMissing = true)
    static class WebClientMessageWriterConfiguration {
    
        /**
         * Web client message writer.
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
         * Netty message writer.
         *
         * @return the message writer
         */
        @Bean
        public MessageWriter nettyMessageWriter() {
            return new NettyClientMessageWriter();
        }
    }
}
