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

package org.apache.shenyu.sync.data.http;

import okhttp3.OkHttpClient;
import okhttp3.Protocol;
import okhttp3.Request;
import okhttp3.Response;
import okhttp3.ResponseBody;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.apache.shenyu.sync.data.http.config.HttpConfig;
import org.apache.shenyu.sync.data.http.refresh.AbstractDataRefresh;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class HttpPluginSnapshotRecoveryTest {

    private static final String SERVER = "http://127.0.0.1:1";

    @Test
    void testRestoresLastAppliedDigestAfterAdminRollback() {
        AtomicReference<String> active = new AtomicReference<>();
        AtomicReference<Snapshot> admin = new AtomicReference<>(new Snapshot("v1", 1L));
        AtomicBoolean failOnce = new AtomicBoolean(true);
        List<String> fetches = new ArrayList<>();
        PluginDataSubscriber subscriber = new PluginDataSubscriber() {
            @Override
            public void refreshPluginDataAll() {
                active.set(null);
            }

            @Override
            public void onSubscribe(final PluginData data) {
                if ("v2".equals(data.getName()) && failOnce.getAndSet(false)) {
                    throw new IllegalStateException("temporary subscriber failure");
                }
                active.set(data.getName());
            }
        };
        OkHttpClient client = new OkHttpClient.Builder().addInterceptor(chain -> {
            Request request = chain.request();
            Snapshot snapshot = admin.get();
            String path = request.url().encodedPath();
            String body;
            if (Constants.SHENYU_ADMIN_PATH_CONFIGS_FETCH.equals(path)) {
                assertEquals(List.of(ConfigGroupEnum.PLUGIN.name()), request.url().queryParameterValues("groupKeys"));
                fetches.add(snapshot.digest());
                body = snapshot.response();
            } else if (Constants.SHENYU_ADMIN_PATH_CONFIGS_LISTENER.equals(path)) {
                String advertised = request.url().queryParameter(ConfigGroupEnum.PLUGIN.name());
                // Admin checks the content digest before the timestamp.
                body = Objects.nonNull(advertised) && advertised.startsWith(snapshot.digest() + ",") ? "{\"data\":[]}" : "{\"data\":[\"PLUGIN\"]}";
            } else {
                throw new AssertionError("Unexpected path: " + path);
            }
            return new Response.Builder().request(request).protocol(Protocol.HTTP_1_1)
                    .code(200).message("OK").body(ResponseBody.create(body, null)).build();
        }).build();
        HttpConfig config = new HttpConfig();
        config.setUrl(SERVER);
        AccessTokenManager tokens = mock(AccessTokenManager.class);
        when(tokens.getAccessToken()).thenReturn("token");
        AtomicBoolean running = (AtomicBoolean) ReflectionTestUtils.getField(HttpSyncDataService.class, "RUNNING");
        Map<?, ?> cache = (Map<?, ?>) ReflectionTestUtils.getField(AbstractDataRefresh.class, "GROUP_CACHE");
        Objects.requireNonNull(cache).clear();
        // Drive each step explicitly without a background poller racing the fake Admin.
        Objects.requireNonNull(running).set(true);
        try (HttpSyncDataService service = new HttpSyncDataService(config, subscriber, client, List.of(), List.of(), List.of(),
                List.of(), List.of(), tokens, new ShenyuConfig())) {
            ReflectionTestUtils.invokeMethod(service, "doFetchGroupConfig", SERVER, new ConfigGroupEnum[]{ConfigGroupEnum.PLUGIN});
            assertEquals("v1", active.get());

            admin.set(new Snapshot("v2", 2L));
            assertThrows(IllegalStateException.class, () -> poll(service));
            assertNull(active.get());

            admin.set(new Snapshot("v1", 3L));
            poll(service);
            assertEquals("v1", active.get());
            assertEquals(List.of("v1", "v2", "v1"), fetches);

            poll(service);
            assertEquals(List.of("v1", "v2", "v1"), fetches);

            admin.set(new Snapshot("v3", 4L));
            poll(service);
            assertEquals("v3", active.get());
        } finally {
            running.set(false);
            cache.clear();
        }
    }

    private void poll(final HttpSyncDataService service) {
        ReflectionTestUtils.invokeMethod(service, "doLongPolling", SERVER);
    }

    private record Snapshot(String digest, long version) {

        private String response() {
            return String.format("{\"data\":{\"PLUGIN\":{\"md5\":\"%s\",\"lastModifyTime\":%d,\"data\":[{\"name\":\"%s\"}]}}}", digest, version, digest);
        }
    }
}
