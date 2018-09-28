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

package org.dromara.soul.bootstrap.http;

import org.dromara.soul.bootstrap.BaseTest;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.test.web.reactive.server.WebTestClient;
import org.springframework.web.reactive.function.BodyInserters;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.core.publisher.Mono;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

/**
 * @author xiaoyu(Myth)
 */
public class HttpTest extends BaseTest {

    /**
     * logger
     */
    private static final Logger LOGGER = LoggerFactory.getLogger(HttpTest.class);


    @Autowired(required = false)
    private WebTestClient webTestClient;

    private static final ExecutorService EXECUTE_SERVICE = Executors.newFixedThreadPool(300);

    @Test
    public void testHttp() throws InterruptedException {
        String body="{\n" +
                "\t\"domain\": \"GB\",\n" +
                "\t\"accessToken\": \"576861360e1344ba9b6de1992f60b28a\",\n" +
                "\t\"version\": \"3.0\",\n" +
                "\t\"language\": \"en\",\n" +
                "\t\"filters\": [{\n" +
                "\t\t\"field\": \"skuId\",\n" +
                "\t\t\"values\": [\"261417302#1349303\"]\n" +
                "\t}]\n" +
                "}";
        for (int i = 0; i < 1; i++) {

            EXECUTE_SERVICE.execute(()->  webTestClient.post()
                    .headers(this::buildGetHttpHeaders)
                    .contentType(MediaType.APPLICATION_JSON_UTF8)
                    .body(BodyInserters.fromObject(body))
                    .accept(MediaType.APPLICATION_JSON_UTF8)
                    .exchange()
                    .expectStatus().isOk()
                    .expectBody(String.class)
                    .consumeWith(System.out::println)
            );
        }

        Thread.sleep(30000L);

    }

    private void buildGetHttpHeaders(HttpHeaders httpHeaders) {
        httpHeaders.set("module", "performance_es");
        httpHeaders.set("method","price");
        httpHeaders.set("httpMethod","post");
        httpHeaders.set("rpcType","http");
        httpHeaders.set("sign","D48C690C1B1FD5447BD4CCFF3F0C69F4");
        httpHeaders.set("appKey","xiaoyu");
        httpHeaders.set("timestamp","2018-06-06 08:30:10");
        httpHeaders.set("extInfo","{\"age\":\"27\",\"name\":\"xiaoyu\"}");
    }


    @Test
    public void test4xxClientError() {
        webTestClient.post()
                .contentType(MediaType.APPLICATION_JSON_UTF8)
                .body(BodyInserters.fromObject(""))
                .accept(MediaType.APPLICATION_JSON_UTF8)
                .exchange()
                .expectStatus().is4xxClientError();

    }



    @Test
    public void testUrlBiulder() {
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
