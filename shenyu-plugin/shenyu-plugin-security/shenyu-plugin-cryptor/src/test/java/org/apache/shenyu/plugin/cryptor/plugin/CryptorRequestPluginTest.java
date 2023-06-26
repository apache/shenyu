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

package org.apache.shenyu.plugin.cryptor.plugin;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.cryptor.handler.CryptorRequestPluginDataHandler;
import org.apache.shenyu.plugin.cryptor.handler.CryptorRuleHandler;
import org.apache.shenyu.plugin.cryptor.strategy.MapTypeEnum;
import org.apache.shenyu.plugin.cryptor.utils.CryptorUtil;
import org.apache.shenyu.plugin.cryptor.utils.JsonUtil;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.reactive.function.server.HandlerStrategies;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Test case for {@link CryptorRequestPlugin}.
 */
@ExtendWith(MockitoExtension.class)
public class CryptorRequestPluginTest {

    private RuleData ruleData;

    @Mock
    private ShenyuPluginChain chain;

    private ServerWebExchange exchange;

    private CryptorRequestPlugin cryptorRequestPlugin;

    private CryptorRequestPluginDataHandler cryptorRequestPluginDataHandler;

    @BeforeEach
    public void setUp() {
        this.ruleData = new RuleData();
        this.ruleData.setSelectorId("test");
        this.ruleData.setName("test-cryptor-request-plugin");
        this.cryptorRequestPluginDataHandler = new CryptorRequestPluginDataHandler();
        this.cryptorRequestPlugin = new CryptorRequestPlugin(HandlerStrategies.builder().build().messageReaders());
    }

    @Test
    public void decryptTest() {
        this.ruleData.setHandle("{\"strategyName\":\"rsa\","
                + "\"fieldNames\":\"inputToken\","
                + "\"decryptKey\":\"MIIBVQIBADANBgkqhkiG9w0BAQEFAASCAT8wggE7AgEAAkEAtrfolGUtLhZVSpd5L/oAXbGW9Rn54mD96Ny"
                + "uWsxp/KCscDoeFScN7uSc3LwKk14wrC4X0+fSDxm0kMPTvgNBywIDAQABAkBFPvt4ycNOlQ4r364A3akn2PbR2s9V2NZBW"
                + "ukE5jVAlOvgCn6L/+tsVDSQgeVtOPd6rwM2a24iASDsNEbnVrwBAiEA34DwAmsa1phE5aGKM1bPHJiGgM8yolIYDWBaBCu"
                + "PTgECIQDRSOWA8rLJWP+Vijm/QB8C41Gw1V7WXC2Kuj07Jv5nywIgTDKCIODw8m5RNtRe8GfNDlu1p158TbidOJo7tiY/og"
                + "ECIQCaj0tvP83qBWA8AClFpQVCDL936RxxEwJPQduWo+WeoQIhAN7HKEW0E97il2RvCsgeArdt83WjZh7OhMhW6MLPrMjs\","
                + "\"encryptKey\":\"MFwwDQYJKoZIhvcNAQEBBQADSwAwSAJBALa36JRlLS4WVUqXeS/6AF2xlvUZ+eJg/ejcrlrMafygrHA6Hh"
                + "UnDe7knNy8CpNeMKwuF9Pn0g8ZtJDD074DQcsCAwEAAQ\\u003d\\u003d\","
                + "\"way\":\"decrypt\","
                + "\"mapType\":\"all\""
                + "}\n");
        this.exchange = MockServerWebExchange.from(MockServerHttpRequest
                .method(HttpMethod.POST, "/test")
                .header(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .body("{\"inputToken\": "
                        + "\"kYPZgOAR2pEipskl5WURW/r3CMxNQJwbs4jbTAOfZNV39L4WkaTOqAeolV+rlKCKiXKvhfHWaxQOTMm9hQBxLA==\""
                        + "}"));
        SelectorData selectorData = mock(SelectorData.class);
        when(this.chain.execute(any())).thenReturn(Mono.empty());
        cryptorRequestPluginDataHandler.handlerRule(ruleData);
        StepVerifier.create(cryptorRequestPlugin.doExecute(this.exchange, this.chain, selectorData, this.ruleData)).expectSubscription().verifyComplete();
    }

    @Test
    public void encryptTest() {
        this.ruleData.setHandle("{\"strategyName\":\"rsa\","
                + "\"fieldNames\":\"inputToken\","
                + "\"decryptKey\":\"MIIBVQIBADANBgkqhkiG9w0BAQEFAASCAT8wggE7AgEAAkEAtrfolGUtLhZVSpd5L/oAXbGW9Rn54mD96Ny"
                + "uWsxp/KCscDoeFScN7uSc3LwKk14wrC4X0+fSDxm0kMPTvgNBywIDAQABAkBFPvt4ycNOlQ4r364A3akn2PbR2s9V2NZBW"
                + "ukE5jVAlOvgCn6L/+tsVDSQgeVtOPd6rwM2a24iASDsNEbnVrwBAiEA34DwAmsa1phE5aGKM1bPHJiGgM8yolIYDWBaBCu"
                + "PTgECIQDRSOWA8rLJWP+Vijm/QB8C41Gw1V7WXC2Kuj07Jv5nywIgTDKCIODw8m5RNtRe8GfNDlu1p158TbidOJo7tiY/og"
                + "ECIQCaj0tvP83qBWA8AClFpQVCDL936RxxEwJPQduWo+WeoQIhAN7HKEW0E97il2RvCsgeArdt83WjZh7OhMhW6MLPrMjs\","
                + "\"encryptKey\":\"MFwwDQYJKoZIhvcNAQEBBQADSwAwSAJBALa36JRlLS4WVUqXeS/6AF2xlvUZ+eJg/ejcrlrMafygrHA6Hh"
                + "UnDe7knNy8CpNeMKwuF9Pn0g8ZtJDD074DQcsCAwEAAQ\\u003d\\u003d\","
                + "\"way\":\"encrypt\","
                + "\"mapType\":\"all\""
                + "}\n");
        this.exchange = MockServerWebExchange.from(MockServerHttpRequest
                .method(HttpMethod.POST, "/test")
                .header(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .body("{\"inputToken\": "
                        + "\"shenyu\""
                        + "}"));
        SelectorData selectorData = mock(SelectorData.class);
        when(this.chain.execute(any())).thenReturn(Mono.empty());
        cryptorRequestPluginDataHandler.handlerRule(ruleData);
        StepVerifier.create(cryptorRequestPlugin.doExecute(this.exchange, this.chain, selectorData, this.ruleData)).expectSubscription().verifyComplete();
    }

    @Test
    public void multiJsonEncryptTest() {
        this.ruleData.setHandle("{\"strategyName\":\"rsa\","
                + "\"fieldNames\":\"inputToken.test\","
                + "\"decryptKey\":\"MIIBVQIBADANBgkqhkiG9w0BAQEFAASCAT8wggE7AgEAAkEAtrfolGUtLhZVSpd5L/oAXbGW9Rn54mD96Ny"
                + "uWsxp/KCscDoeFScN7uSc3LwKk14wrC4X0+fSDxm0kMPTvgNBywIDAQABAkBFPvt4ycNOlQ4r364A3akn2PbR2s9V2NZBW"
                + "ukE5jVAlOvgCn6L/+tsVDSQgeVtOPd6rwM2a24iASDsNEbnVrwBAiEA34DwAmsa1phE5aGKM1bPHJiGgM8yolIYDWBaBCu"
                + "PTgECIQDRSOWA8rLJWP+Vijm/QB8C41Gw1V7WXC2Kuj07Jv5nywIgTDKCIODw8m5RNtRe8GfNDlu1p158TbidOJo7tiY/og"
                + "ECIQCaj0tvP83qBWA8AClFpQVCDL936RxxEwJPQduWo+WeoQIhAN7HKEW0E97il2RvCsgeArdt83WjZh7OhMhW6MLPrMjs\","
                + "\"encryptKey\":\"MFwwDQYJKoZIhvcNAQEBBQADSwAwSAJBALa36JRlLS4WVUqXeS/6AF2xlvUZ+eJg/ejcrlrMafygrHA6Hh"
                + "UnDe7knNy8CpNeMKwuF9Pn0g8ZtJDD074DQcsCAwEAAQ\\u003d\\u003d\","
                + "\"way\":\"encrypt\","
                + "\"mapType\":\"all\""
                + "}\n");
        this.exchange = MockServerWebExchange.from(MockServerHttpRequest
                .method(HttpMethod.POST, "/test")
                .header(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .body("{\"inputToken\": "
                        + "{\"test\":\"shenyu\"}"
                        + "}"));
        SelectorData selectorData = mock(SelectorData.class);
        when(this.chain.execute(any())).thenReturn(Mono.empty());
        cryptorRequestPluginDataHandler.handlerRule(ruleData);
        StepVerifier.create(cryptorRequestPlugin.doExecute(this.exchange, this.chain, selectorData, this.ruleData)).expectSubscription().verifyComplete();
    }

    @Test
    public void multiJsonDecryptTest() {
        this.ruleData.setHandle("{\"strategyName\":\"rsa\","
                + "\"fieldNames\":\"inputToken.test\","
                + "\"decryptKey\":\"MIIBVQIBADANBgkqhkiG9w0BAQEFAASCAT8wggE7AgEAAkEAtrfolGUtLhZVSpd5L/oAXbGW9Rn54mD96Ny"
                + "uWsxp/KCscDoeFScN7uSc3LwKk14wrC4X0+fSDxm0kMPTvgNBywIDAQABAkBFPvt4ycNOlQ4r364A3akn2PbR2s9V2NZBW"
                + "ukE5jVAlOvgCn6L/+tsVDSQgeVtOPd6rwM2a24iASDsNEbnVrwBAiEA34DwAmsa1phE5aGKM1bPHJiGgM8yolIYDWBaBCu"
                + "PTgECIQDRSOWA8rLJWP+Vijm/QB8C41Gw1V7WXC2Kuj07Jv5nywIgTDKCIODw8m5RNtRe8GfNDlu1p158TbidOJo7tiY/og"
                + "ECIQCaj0tvP83qBWA8AClFpQVCDL936RxxEwJPQduWo+WeoQIhAN7HKEW0E97il2RvCsgeArdt83WjZh7OhMhW6MLPrMjs\","
                + "\"encryptKey\":\"MFwwDQYJKoZIhvcNAQEBBQADSwAwSAJBALa36JRlLS4WVUqXeS/6AF2xlvUZ+eJg/ejcrlrMafygrHA6Hh"
                + "UnDe7knNy8CpNeMKwuF9Pn0g8ZtJDD074DQcsCAwEAAQ\\u003d\\u003d\","
                + "\"way\":\"decrypt\","
                + "\"mapType\":\"field\""
                + "}\n");
        this.exchange = MockServerWebExchange.from(MockServerHttpRequest
                .method(HttpMethod.POST, "/test")
                .header(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .body("{\"inputToken\": "
                        + "{\"test\":\"kYPZgOAR2pEipskl5WURW/r3CMxNQJwbs4jbTAOfZNV39L4WkaTOqAeolV+rlKCKiXKvhfHWaxQOTMm9hQBxLA==\"}"
                        + "}"));
        SelectorData selectorData = mock(SelectorData.class);
        when(this.chain.execute(any())).thenReturn(Mono.empty());
        cryptorRequestPluginDataHandler.handlerRule(ruleData);
        StepVerifier.create(cryptorRequestPlugin.doExecute(this.exchange, this.chain, selectorData, this.ruleData)).expectSubscription().verifyComplete();
    }

    @Test
    public void namedTest() {
        final String result = cryptorRequestPlugin.named();
        assertEquals(PluginEnum.CRYPTOR_REQUEST.getName(), result);
    }

    @Test
    public void getOrderTest() {
        final int result = cryptorRequestPlugin.getOrder();
        assertEquals(PluginEnum.CRYPTOR_REQUEST.getCode(), result);
    }
    
    @Test
    public void mapTypeDecryptFieldTest() {
        this.ruleData.setHandle("{\"strategyName\":\"rsa\","
                + "\"fieldNames\":\"inputToken.test\","
                + "\"decryptKey\":\"MIIBVQIBADANBgkqhkiG9w0BAQEFAASCAT8wggE7AgEAAkEAtrfolGUtLhZVSpd5L/oAXbGW9Rn54mD96Ny"
                + "uWsxp/KCscDoeFScN7uSc3LwKk14wrC4X0+fSDxm0kMPTvgNBywIDAQABAkBFPvt4ycNOlQ4r364A3akn2PbR2s9V2NZBW"
                + "ukE5jVAlOvgCn6L/+tsVDSQgeVtOPd6rwM2a24iASDsNEbnVrwBAiEA34DwAmsa1phE5aGKM1bPHJiGgM8yolIYDWBaBCu"
                + "PTgECIQDRSOWA8rLJWP+Vijm/QB8C41Gw1V7WXC2Kuj07Jv5nywIgTDKCIODw8m5RNtRe8GfNDlu1p158TbidOJo7tiY/og"
                + "ECIQCaj0tvP83qBWA8AClFpQVCDL936RxxEwJPQduWo+WeoQIhAN7HKEW0E97il2RvCsgeArdt83WjZh7OhMhW6MLPrMjs\","
                + "\"encryptKey\":\"MFwwDQYJKoZIhvcNAQEBBQADSwAwSAJBALa36JRlLS4WVUqXeS/6AF2xlvUZ+eJg/ejcrlrMafygrHA6Hh"
                + "UnDe7knNy8CpNeMKwuF9Pn0g8ZtJDD074DQcsCAwEAAQ\\u003d\\u003d\","
                + "\"way\":\"decrypt\","
                + "\"mapType\":\"field\""
                + "}\n");
        final String originalBody = "{\"inputToken\": "
                + "{\"test\":\"kYPZgOAR2pEipskl5WURW/r3CMxNQJwbs4jbTAOfZNV39L4WkaTOqAeolV+rlKCKiXKvhfHWaxQOTMm9hQBxLA==\"}"
                + "}";
        this.exchange = MockServerWebExchange.from(MockServerHttpRequest
                .method(HttpMethod.POST, "/test")
                .header(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .body(originalBody));
        cryptorRequestPluginDataHandler.handlerRule(ruleData);
        CryptorRuleHandler ruleHandle = CryptorRequestPluginDataHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(ruleData));
        String parseBody = JsonUtil.parser(originalBody, ruleHandle.getFieldNames());
        assertEquals(CryptorUtil.crypt(ruleHandle, parseBody, originalBody, exchange), "{\"nickName\":\"openApi\"}");
    }

    @Test
    public void mapTypeDecryptAllTest() {
        this.ruleData.setHandle("{\"strategyName\":\"rsa\","
                + "\"fieldNames\":\"inputToken.test\","
                + "\"decryptKey\":\"MIIBVQIBADANBgkqhkiG9w0BAQEFAASCAT8wggE7AgEAAkEAtrfolGUtLhZVSpd5L/oAXbGW9Rn54mD96Ny"
                + "uWsxp/KCscDoeFScN7uSc3LwKk14wrC4X0+fSDxm0kMPTvgNBywIDAQABAkBFPvt4ycNOlQ4r364A3akn2PbR2s9V2NZBW"
                + "ukE5jVAlOvgCn6L/+tsVDSQgeVtOPd6rwM2a24iASDsNEbnVrwBAiEA34DwAmsa1phE5aGKM1bPHJiGgM8yolIYDWBaBCu"
                + "PTgECIQDRSOWA8rLJWP+Vijm/QB8C41Gw1V7WXC2Kuj07Jv5nywIgTDKCIODw8m5RNtRe8GfNDlu1p158TbidOJo7tiY/og"
                + "ECIQCaj0tvP83qBWA8AClFpQVCDL936RxxEwJPQduWo+WeoQIhAN7HKEW0E97il2RvCsgeArdt83WjZh7OhMhW6MLPrMjs\","
                + "\"encryptKey\":\"MFwwDQYJKoZIhvcNAQEBBQADSwAwSAJBALa36JRlLS4WVUqXeS/6AF2xlvUZ+eJg/ejcrlrMafygrHA6Hh"
                + "UnDe7knNy8CpNeMKwuF9Pn0g8ZtJDD074DQcsCAwEAAQ\\u003d\\u003d\","
                + "\"way\":\"decrypt\","
                + "\"mapType\":\"all\""
                + "}\n");
        final String originalBody = "{\"inputToken\": "
                + "{\"test\":\"kYPZgOAR2pEipskl5WURW/r3CMxNQJwbs4jbTAOfZNV39L4WkaTOqAeolV+rlKCKiXKvhfHWaxQOTMm9hQBxLA==\"}"
                + "}";
        this.exchange = MockServerWebExchange.from(MockServerHttpRequest
                .method(HttpMethod.POST, "/test")
                .header(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .body(originalBody));
        cryptorRequestPluginDataHandler.handlerRule(ruleData);
        CryptorRuleHandler ruleHandle = CryptorRequestPluginDataHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(ruleData));
        String parseBody = JsonUtil.parser(originalBody, ruleHandle.getFieldNames());
        assertEquals(CryptorUtil.crypt(ruleHandle, parseBody, originalBody, exchange), "{\"inputToken\":{\"test\":\"{\\\"nickName\\\":\\\"openApi\\\"}\"}}");
    }

    @Test
    public void mapTypeDecryptMultFieldsTest() {
        this.ruleData.setHandle("{\"strategyName\":\"rsa\","
                + "\"fieldNames\":\"inputToken.one,inputToken.two\","
                + "\"decryptKey\":\"MIIBVQIBADANBgkqhkiG9w0BAQEFAASCAT8wggE7AgEAAkEAtrfolGUtLhZVSpd5L/oAXbGW9Rn54mD96Ny"
                + "uWsxp/KCscDoeFScN7uSc3LwKk14wrC4X0+fSDxm0kMPTvgNBywIDAQABAkBFPvt4ycNOlQ4r364A3akn2PbR2s9V2NZBW"
                + "ukE5jVAlOvgCn6L/+tsVDSQgeVtOPd6rwM2a24iASDsNEbnVrwBAiEA34DwAmsa1phE5aGKM1bPHJiGgM8yolIYDWBaBCu"
                + "PTgECIQDRSOWA8rLJWP+Vijm/QB8C41Gw1V7WXC2Kuj07Jv5nywIgTDKCIODw8m5RNtRe8GfNDlu1p158TbidOJo7tiY/og"
                + "ECIQCaj0tvP83qBWA8AClFpQVCDL936RxxEwJPQduWo+WeoQIhAN7HKEW0E97il2RvCsgeArdt83WjZh7OhMhW6MLPrMjs\","
                + "\"encryptKey\":\"MFwwDQYJKoZIhvcNAQEBBQADSwAwSAJBALa36JRlLS4WVUqXeS/6AF2xlvUZ+eJg/ejcrlrMafygrHA6Hh"
                + "UnDe7knNy8CpNeMKwuF9Pn0g8ZtJDD074DQcsCAwEAAQ\\u003d\\u003d\","
                + "\"way\":\"decrypt\","
                + "\"mapType\":\"all\""
                + "}\n");
        final String originalBody = "{\"inputToken\": "
                + "{\"one\":\"kYPZgOAR2pEipskl5WURW/r3CMxNQJwbs4jbTAOfZNV39L4WkaTOqAeolV+rlKCKiXKvhfHWaxQOTMm9hQBxLA==\"," 
                + "\"two\":\"kYPZgOAR2pEipskl5WURW/r3CMxNQJwbs4jbTAOfZNV39L4WkaTOqAeolV+rlKCKiXKvhfHWaxQOTMm9hQBxLA==\"}"
                + "}";
        this.exchange = MockServerWebExchange.from(MockServerHttpRequest
                .method(HttpMethod.POST, "/test")
                .header(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .body(originalBody));
        cryptorRequestPluginDataHandler.handlerRule(ruleData);
        CryptorRuleHandler ruleHandle = CryptorRequestPluginDataHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(ruleData));
        String parseBody = MapTypeEnum.mapType(ruleHandle.getMapType()).convert(originalBody, ruleHandle, exchange);
        assertEquals(parseBody, "{\"inputToken\":{\"one\":\"{\\\"nickName\\\":\\\"openApi\\\"}\",\"two\":\"{\\\"nickName\\\":\\\"openApi\\\"}\"}}");
    }
    
}
