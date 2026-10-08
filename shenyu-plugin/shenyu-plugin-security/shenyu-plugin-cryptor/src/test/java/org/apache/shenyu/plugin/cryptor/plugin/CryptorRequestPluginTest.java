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

    // 2048-bit RSA keypair (OAEP/SHA-256 needs a key large enough to hold the
    // plaintext after padding; the legacy 512-bit fixture cannot). RSA_CIPHERTEXT
    // is the OAEP encryption of {"nickName":"openApi"} under ENC_KEY.
    private static final String ENC_KEY = "MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAtxADsITceaH5ubXIISZHpRU7nH89rVzbxkp9l9u3Qr3NAYCrx5kOffMtiik/ndD6iCTusKrJkGqJqmkgT3V2PG/o72FMvxGMQGgI6X+Lwr"
            + "WMuShiF/WBB1aEirII1151J9L6vBzr2JxAb96612CYgB4ZodYW9my569UI0DovLP68L29VS4r+Zndxx3C3EASfdjllgPHysZWIv8iA2t4g7Zap/xnHNIgEJ3MC50nl7gtu+I3aTF6WV/SkzhxZat4G"
            + "EXfzErDfoFvZwVqtRTwG6SDtdxXPpMGWUELOdICVr9hJ4sNKbIacjEuwTvdf9w9sRrTv//Um+8fg9uUS3e9xMQIDAQAB";

    private static final String DEC_KEY = "MIIEvQIBADANBgkqhkiG9w0BAQEFAASCBKcwggSjAgEAAoIBAQC3EAOwhNx5ofm5tcghJkelFTucfz2tXNvGSn2X27dCvc0BgKvHmQ598y2KKT+d0PqIJO6wqsmQaomqaSBPdXY8b+jvYUy/EYxAaA"
            + "jpf4vCtYy5KGIX9YEHVoSKsgjXXnUn0vq8HOvYnEBv3rrXYJiAHhmh1hb2bLnr1QjQOi8s/rwvb1VLiv5md3HHcLcQBJ92OWWA8fKxlYi/yIDa3iDtlqn/Gcc0iAQncwLnSeXuC274jdpMXpZX9KTO"
            + "HFlq3gYRd/MSsN+gW9nBWq1FPAbpIO13Fc+kwZZQQs50gJWv2Eniw0pshpyMS7BO91/3D2xGtO//9Sb7x+D25RLd73ExAgMBAAECggEADQvyY154sx+A44Qp3Pj0MrcCblsgK26aiDWPYWcKltJdnb"
            + "2MoJdPMdlGtdnOO6Jk9JaDP2qQnn8FTDSdVaRmtpR4OrVJybVHtGBlwDRzor8bJigTY6c+2KXJIPRizmygN2QhNA5wnZm3OvHaCZcMD1d11rOiI9JoZr8iV2rKKW/n+qyNckJTFNC88O5RFxmdMx2Q"
            + "6iGoSS3LGdXxCKhdKXdzGwD2cFDSIMMFCiO2SJDfzyj6mWVZipnduFuc2q9jD4nIcxeDXv1+wYHkUV301CJYvm/Cluw10+pJB193fuC15ZMMrOEuMtTWLAyG0BLSsU6831aqZHhra2RJw4Q2sQKBgQ"
            + "D+KkYhpep0vHDwvpq26cpvvj34f4BdevKPZxhu5INkQiVBffIfo/WiWgBMn6IrE+Mp2hf7HvvYmQNIDpW15fhutxnKhM4AnaGLNaWndSa/EiLNcATmVD9i+RowA+ZpELtWiG6pstn/DP5C17ttPhY5"
            + "wPCVxqN0amHUuBL+yo8JWQKBgQC4YlW2wsjRSsblyryYae7Orh2ZAa+2rvO/Vmkuf684tlEGlyg0ruF5i0kKYDkrl9ffLAmN+/3hzJ9ur5XrO/zGlsMsIyy+A+1cnitykUU93A2L//fSME4zR14PmT"
            + "PPyIYZxa1kLcEcDRCByuO2glk/44kfChzVygNbMdVKOWNTmQKBgQCm0QwyrXkSoVPnTtKw1wV9DfoSjWys7jMhl+LbdbQfK6LUN1uhFLX1lui3YdbIO0dPgstWkOFvKg6TTq9IMeY6lIai+0NR+CO9"
            + "ALr3C9cgdUDOYYV1vznTNffQJ98kekza4LTxQGgAFIEVUg68BpID2fSN+U/y6pfHTAF7pWr4EQKBgAYqw9MpEK5vYdetwEEYyfP/vt2vQMFLeLudmEcF3kZ3Up51z9JzRvdZwUenkEH1AjNkta0aEJ"
            + "PM1EhPdyQ3DW1W/ZAsXQK9/uJqJ+ndEgPPqGRWW2OcWgE9EdhTt3frrRCPnA0NurfFeBffQV6JXZLVeXCgVfaQmywhrpCc+sWBAoGAZIk5MEk+zrQt3CrH2UC/ly+1/DsrwAtYpWFlLR7KmFL9p+X8"
            + "rF6NdeaqiPNZ7KFLB+veOyriNRQ7oT3i8zmd2uv+DVokxlZ/QowBgUd2AGBudX5unAT0lEykf+4hK7mrvkePv+K2UsxPm+BfpIDI7ggq+4SWeDcx7OqGt6rU3Xc=";

    private static final String RSA_CIPHERTEXT = "KMyfRryRP2XUPnXAVa+nuXf0jM7VDJYOFkim2aeP0Ar5j9RhuhX9g2ozG9B1AfVmBGN+HoU8qTEzfo1i3/sz6e3Fxb/nsoe8oxdh5F/J44pa3XHqwBbL14hsvBizt6eDuOIXFULcTQNtIexBvMg6t"
            + "tgSPOZWZzm1Z0/gFsCkNT/8qX1S2HUPwJnl2L9h+MlNDNgSp1E+Zqu2f/UnJTTqCDtsoDRIwuK2fnLZOuzBiN27Du81kbcjJbmExGHj8qMsItF5c0EjvG138RImMFFdcBh+2VA0LEQ84NRrxS+HNcDf"
            + "/lycLNFa+XJWYfwmRmP1D2uuDkFBHNNMTMSZn2vx2w==";

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
                + "\"decryptKey\":\"" + DEC_KEY + "\","
                + "\"encryptKey\":\"" + ENC_KEY + "\","
                + "\"way\":\"decrypt\","
                + "\"mapType\":\"all\""
                + "}\n");
        this.exchange = MockServerWebExchange.from(MockServerHttpRequest
                .method(HttpMethod.POST, "/test")
                .header(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .body("{\"inputToken\": \"" + RSA_CIPHERTEXT + "\"}"));
        SelectorData selectorData = mock(SelectorData.class);
        when(this.chain.execute(any())).thenReturn(Mono.empty());
        cryptorRequestPluginDataHandler.handlerRule(ruleData);
        StepVerifier.create(cryptorRequestPlugin.doExecute(this.exchange, this.chain, selectorData, this.ruleData)).expectSubscription().verifyComplete();
    }

    @Test
    public void encryptTest() {
        this.ruleData.setHandle("{\"strategyName\":\"rsa\","
                + "\"fieldNames\":\"inputToken\","
                + "\"decryptKey\":\"" + DEC_KEY + "\","
                + "\"encryptKey\":\"" + ENC_KEY + "\","
                + "\"way\":\"encrypt\","
                + "\"mapType\":\"all\""
                + "}\n");
        this.exchange = MockServerWebExchange.from(MockServerHttpRequest
                .method(HttpMethod.POST, "/test")
                .header(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .body("{\"inputToken\": \"shenyu\"}"));
        SelectorData selectorData = mock(SelectorData.class);
        when(this.chain.execute(any())).thenReturn(Mono.empty());
        cryptorRequestPluginDataHandler.handlerRule(ruleData);
        StepVerifier.create(cryptorRequestPlugin.doExecute(this.exchange, this.chain, selectorData, this.ruleData)).expectSubscription().verifyComplete();
    }

    @Test
    public void multiJsonEncryptTest() {
        this.ruleData.setHandle("{\"strategyName\":\"rsa\","
                + "\"fieldNames\":\"inputToken.test\","
                + "\"decryptKey\":\"" + DEC_KEY + "\","
                + "\"encryptKey\":\"" + ENC_KEY + "\","
                + "\"way\":\"encrypt\","
                + "\"mapType\":\"all\""
                + "}\n");
        this.exchange = MockServerWebExchange.from(MockServerHttpRequest
                .method(HttpMethod.POST, "/test")
                .header(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .body("{\"inputToken\": {\"test\":\"shenyu\"}}"));
        SelectorData selectorData = mock(SelectorData.class);
        when(this.chain.execute(any())).thenReturn(Mono.empty());
        cryptorRequestPluginDataHandler.handlerRule(ruleData);
        StepVerifier.create(cryptorRequestPlugin.doExecute(this.exchange, this.chain, selectorData, this.ruleData)).expectSubscription().verifyComplete();
    }

    @Test
    public void multiJsonDecryptTest() {
        this.ruleData.setHandle("{\"strategyName\":\"rsa\","
                + "\"fieldNames\":\"inputToken.test\","
                + "\"decryptKey\":\"" + DEC_KEY + "\","
                + "\"encryptKey\":\"" + ENC_KEY + "\","
                + "\"way\":\"decrypt\","
                + "\"mapType\":\"field\""
                + "}\n");
        this.exchange = MockServerWebExchange.from(MockServerHttpRequest
                .method(HttpMethod.POST, "/test")
                .header(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .body("{\"inputToken\": {\"test\":\"" + RSA_CIPHERTEXT + "\"}}"));
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
                + "\"decryptKey\":\"" + DEC_KEY + "\","
                + "\"encryptKey\":\"" + ENC_KEY + "\","
                + "\"way\":\"decrypt\","
                + "\"mapType\":\"field\""
                + "}\n");
        final String originalBody = "{\"inputToken\": {\"test\":\"" + RSA_CIPHERTEXT + "\"}}";
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
                + "\"decryptKey\":\"" + DEC_KEY + "\","
                + "\"encryptKey\":\"" + ENC_KEY + "\","
                + "\"way\":\"decrypt\","
                + "\"mapType\":\"all\""
                + "}\n");
        final String originalBody = "{\"inputToken\": {\"test\":\"" + RSA_CIPHERTEXT + "\"}}";
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
                + "\"decryptKey\":\"" + DEC_KEY + "\","
                + "\"encryptKey\":\"" + ENC_KEY + "\","
                + "\"way\":\"decrypt\","
                + "\"mapType\":\"all\""
                + "}\n");
        final String originalBody = "{\"inputToken\": {\"one\":\"" + RSA_CIPHERTEXT + "\",\"two\":\"" + RSA_CIPHERTEXT + "\"}}";
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
