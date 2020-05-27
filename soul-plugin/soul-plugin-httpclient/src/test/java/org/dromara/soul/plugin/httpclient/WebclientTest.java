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

package org.dromara.soul.plugin.httpclient;

import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.core.publisher.Mono;

/**
 * The type Webclient test.
 *
 * @author xiaoyu(Myth)
 */
public class WebclientTest {
    
    private static final Logger LOGGER = LoggerFactory.getLogger(WebclientTest.class);
    
    /**
     * Test web client.
     */
    @Test
    public void testWebClient() {
        Mono<String> resp = WebClient.create()
                .get()
                .uri(uriBuilder -> uriBuilder
                        .scheme("http")
                        .host("www.baidu.com")
                        .path("/s")
                        .queryParam("wd", "北京天气")
                        .queryParam("other", "test")
                        .build())
                .retrieve()
                .bodyToMono(String.class);
        LOGGER.info("result:{}", resp.block());
        
    }
}
