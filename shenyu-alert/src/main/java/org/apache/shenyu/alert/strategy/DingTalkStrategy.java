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

package org.apache.shenyu.alert.strategy;

import okhttp3.MediaType;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.RequestBody;
import org.apache.shenyu.alert.DingTalkProp;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.spi.Join;

import javax.crypto.Mac;
import javax.crypto.spec.SecretKeySpec;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.util.Base64;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.TimeUnit;

/**
 * dingTalk strategy.
 */
@Join
public class DingTalkStrategy implements AlertStrategy {

    private static final OkHttpClient OK_HTTP_CLIENT = new OkHttpClient()
            .newBuilder().connectTimeout(50L, TimeUnit.SECONDS)
            .readTimeout(60L, TimeUnit.SECONDS)
            .build();

    @Override
    public void execute(final String handle) throws Exception {

        DingTalkProp dingTalkProp = GsonUtils.getInstance().fromJson(handle, DingTalkProp.class);

        RequestBody body = RequestBody.create(
                MediaType.parse("application/json"), toJson(dingTalkProp));
        Request request = new Request.Builder()
                .url(dingTalkProp.getUrl())
                .post(body)
                .build();
        OK_HTTP_CLIENT.newCall(request).execute();
    }

    private String url(final String secret, final String url) {
        if (secret == null) {
            return url;
        }
        long timeMillis = System.currentTimeMillis();
        String sign = sign(secret, timeMillis);
        if (sign == null) {
            return url;
        }
        return url + "&timestamp=" + timeMillis + "&sign=" + sign;
    }

    private String sign(final String secret, final long timestamp) {

        String stringToSign = timestamp + "\n" + secret;
        try {
            Mac mac = Mac.getInstance("HmacSHA256");
            mac.init(new SecretKeySpec(secret.getBytes(StandardCharsets.UTF_8), "HmacSHA256"));
            byte[] signData = mac.doFinal(stringToSign.getBytes(StandardCharsets.UTF_8));
            return URLEncoder.encode(Base64.getMimeEncoder().encodeToString(signData),"UTF-8");
        } catch (NoSuchAlgorithmException | InvalidKeyException | UnsupportedEncodingException ignored) {
        }
        return null;
    }

    private String toJson(final DingTalkProp prop) {

        Map<String, String> contentMap = new HashMap<>(4);
        contentMap.put("content", prop.getContent());

        Map<String, Object> atMap = new HashMap<>(8);
        atMap.put("atMobiles", prop.getAtMobiles());
        atMap.put("atUserIds", prop.getAtUserIds());
        atMap.put("isAtAll", prop.getAtAll());

        Map<String, Object> body = new HashMap<>(8);

        body.put("at", atMap);
        body.put("msgtype", "text");
        body.put("text", contentMap);
        return GsonUtils.getInstance().toJson(body);
    }

}
