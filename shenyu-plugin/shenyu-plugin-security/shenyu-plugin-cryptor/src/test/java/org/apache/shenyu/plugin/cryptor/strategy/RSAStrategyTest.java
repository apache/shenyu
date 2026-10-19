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

package org.apache.shenyu.plugin.cryptor.strategy;

import org.junit.jupiter.api.Test;

import java.util.Base64;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

public class RSAStrategyTest {

    private final String encKey = "MFwwDQYJKoZIhvcNAQEBBQADSwAwSAJBALa36JRlLS4WVUqXeS/6AF2xlvUZ+eJg/ejcrlrMafygrHA6HhUnDe7knNy8CpNeMKwuF9Pn0g8ZtJDD074DQcsCAwEAAQ";

    private final String decKey = "MIIBVQIBADANBgkqhkiG9w0BAQEFAASCAT8wggE7AgEAAkEAtrfolGUtLhZVSpd5L/oAXbGW9Rn54mD96NyuWsxp/KCscDoeFScN7uSc3LwKk14wrC4X0+fSDxm0kMPT"
            + "vgNBywIDAQABAkBFPvt4ycNOlQ4r364A3akn2PbR2s9V2NZBWukE5jVAlOvgCn6L/+tsVDSQgeVtOPd6rwM2a24iASDsNEbnVrwBAiEA34DwAmsa1phE5aGKM1bPHJiGgM8yolIYDWBaBCuPTgECIQDR"
            + "SOWA8rLJWP+Vijm/QB8C41Gw1V7WXC2Kuj07Jv5nywIgTDKCIODw8m5RNtRe8GfNDlu1p158TbidOJo7tiY/ogECIQCaj0tvP83qBWA8AClFpQVCDL936RxxEwJPQduWo+WeoQIhAN7HKEW0E97il2RvCsgeArdt83WjZh7OhMhW6MLPrMjs";

    private final CryptorStrategy cryptorStrategy = new RsaStrategy();

    private final byte[] encryptedData = Base64.getMimeDecoder().decode("pB+N5TUdavjwxQZaMItpad5kEXgFJwlN02FfNnz97Zx6vXfxoVTAuLyrlTde72Sep7dBu/BEfl5ubRWpXmdsUA==");

    private final String decryptedData = "shenyu";

    @Test
    public void testEncrypt() throws Exception {
        byte[] encryptedData = Base64.getMimeDecoder().decode(cryptorStrategy.encrypt(encKey, decryptedData));
        assertThat(cryptorStrategy.decrypt(decKey, encryptedData), is(decryptedData));
    }

    @Test
    public void testDecrypt() throws Exception {
        assertThat(cryptorStrategy.decrypt(decKey, encryptedData), is(decryptedData));
    }
}
