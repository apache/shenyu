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

/**
 * Verifies the {@code rsa-pkcs1} (PKCS#1 v1.5) strategy round-trips. PKCS#1 v1.5
 * has no authentication tag, so no tamper-detection assertion is asserted here —
 * that property is covered by {@link RSAStrategyTest} for the OAEP default.
 */
public class RsaPkcs1StrategyTest {

    // Same 2048-bit fixture used across the RSA tests.
    private final String encKey = "MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAtxADsITceaH5ubXIISZHpRU7nH89rVzbxkp9l9u3Qr3NAYCrx5kOffMtiik/ndD6iCTusKrJkGqJqmkgT3V2PG/o72FMvxGMQGgI6X+Lwr"
            + "WMuShiF/WBB1aEirII1151J9L6vBzr2JxAb96612CYgB4ZodYW9my569UI0DovLP68L29VS4r+Zndxx3C3EASfdjllgPHysZWIv8iA2t4g7Zap/xnHNIgEJ3MC50nl7gtu+I3aTF6WV/SkzhxZat4G"
            + "EXfzErDfoFvZwVqtRTwG6SDtdxXPpMGWUELOdICVr9hJ4sNKbIacjEuwTvdf9w9sRrTv//Um+8fg9uUS3e9xMQIDAQAB";

    private final String decKey = "MIIEvQIBADANBgkqhkiG9w0BAQEFAASCBKcwggSjAgEAAoIBAQC3EAOwhNx5ofm5tcghJkelFTucfz2tXNvGSn2X27dCvc0BgKvHmQ598y2KKT+d0PqIJO6wqsmQaomqaSBPdXY8b+jvYUy/EYxAaA"
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

    private final CryptorStrategy cryptorStrategy = new RsaPkcs1Strategy();

    @Test
    public void shouldRoundTripUnderPkcs1V15() throws Exception {
        String plaintext = "shenyu";
        byte[] encrypted = Base64.getDecoder().decode(cryptorStrategy.encrypt(encKey, plaintext));
        assertThat(cryptorStrategy.decrypt(decKey, encrypted), is(plaintext));
    }
}
