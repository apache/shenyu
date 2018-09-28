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

package org.dromara.soul.bootstrap.dubbo;

import org.dromara.soul.bootstrap.BaseTest;
import org.dromara.soul.bootstrap.http.HttpTest;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.test.web.reactive.server.WebTestClient;
import org.springframework.web.reactive.function.BodyInserters;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

/**
 * @author xiaoyu(Myth)
 */
public class DubboTest extends BaseTest {

    /**
     * logger
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(HttpTest.class);


    @Autowired(required = false)
    private WebTestClient webTestClient;

    private static final ExecutorService EXECUTE_SERVICE = Executors.newFixedThreadPool(20);

    @Test
    public void testDubbo() throws InterruptedException {

        String request="{\"rpcType\": \"dubbo\"," +
                " \"module\": \"pdm\", \"appKey\": \"gateway\", \"method\": \"helloWorld\", " +
                "\"sign\": \"29E6EDC0331B799E7EED3CAE98E37B62\", " +
                "\"timestamp\": \"2018-05-14 09:20:10\", " +
                "\"content\": {\"interfaceName\": \"com.hqyg.skyway.test.dubbo.api.service.DubboTestService\"," +
                "                \"method\": \"findByIdAndName\",\"timeout\": \"50000\", " +
                "              \"paramClass\": [\"com.hqyg.skyway.test.dubbo.api.entity.DubboTest\",  \"com.hqyg.skyway.test.dubbo.api.entity.DubboTest\"], \"classParams\": [{ \"id\": \"1\",\"name\": \"x\" } ] }}";
        for (int i = 0; i < 10000; i++) {

            EXECUTE_SERVICE.execute(()->  webTestClient.post()
                    .contentType(MediaType.APPLICATION_JSON_UTF8)
                    .body(BodyInserters.fromObject(request))
                    .accept(MediaType.APPLICATION_JSON_UTF8)
                    .exchange()
                    .expectStatus().isOk()
                    .expectBody(String.class)
                    .consumeWith(System.out::println)
            );
        }

        Thread.sleep(30000L);

    }

}
