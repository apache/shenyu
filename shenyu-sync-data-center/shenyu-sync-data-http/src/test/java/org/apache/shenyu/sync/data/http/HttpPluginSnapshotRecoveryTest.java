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
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.apache.shenyu.sync.data.http.config.HttpConfig;
import org.apache.shenyu.sync.data.http.refresh.AbstractDataRefresh;
import org.apache.shenyu.sync.data.http.refresh.PluginDataRefresh;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class HttpPluginSnapshotRecoveryTest {

    private static final String SERVER = "http://127.0.0.1:1";

    private static final String HEALTHY_PEER = "http://127.0.0.1:2";

    @Test
    void testRestoresLastAppliedDigestAfterAdminRollback() {
        try (Fixture fixture = new Fixture()) {
            fixture.failAfterClearingPlugins();
            fixture.admin = new Snapshot("v1", 7L);
            fixture.poll(SERVER);
            assertEquals("v1", fixture.active);
            assertEquals(List.of(SERVER, SERVER, SERVER), fixture.pluginFetches);
            fixture.poll(SERVER);
            assertEquals(3, fixture.pluginFetches.size());

            fixture.admin = new Snapshot("v3", 8L);
            fixture.poll(SERVER);
            assertEquals("v3", fixture.active);
        }
    }

    @Test
    void testPluginCallbackFailureDoesNotBlockOtherGroupsInSamePoll() {
        try (Fixture fixture = new Fixture()) {
            fixture.failAfterClearingPlugins();
            fixture.metadataChanged = true;
            fixture.poll(SERVER);
            verify(fixture.metadata).refresh();
            assertNull(fixture.active);
            assertEquals("v1,5", fixture.pluginVersion);
            assertEquals(3, fixture.pluginFetches.size());
        }
    }

    @Test
    void testPluginHttpFailureDoesNotBlockPollingOrThrottleHealthyPeer() {
        try (Fixture fixture = new Fixture()) {
            fixture.failAfterClearingPlugins();
            fixture.admin = new Snapshot("v1", 3L);
            fixture.metadataChanged = true;
            fixture.failServer = true;
            fixture.poll(SERVER);
            verify(fixture.metadata).refresh();
            assertNull(fixture.active);

            fixture.poll(HEALTHY_PEER);
            assertEquals("v1", fixture.active);
            fixture.poll(HEALTHY_PEER);
            assertEquals("v1,5", fixture.pluginVersion);
            assertEquals(List.of(SERVER, SERVER, SERVER, HEALTHY_PEER), fixture.pluginFetches);
        }
    }

    @Test
    void testRestoresSameDigestWithoutRegressingPublishedVersion() {
        try (Fixture fixture = new Fixture()) {
            fixture.failAfterClearingPlugins();
            fixture.admin = new Snapshot("v1", 3L);
            fixture.poll(SERVER);
            assertEquals("v1", fixture.active);
            fixture.poll(SERVER);
            assertEquals("v1,5", fixture.pluginVersion);
            assertEquals(3, fixture.pluginFetches.size());

            fixture.admin = new Snapshot("stale", 4L);
            fixture.poll(SERVER);
            assertEquals("v1", fixture.active);
        }
    }

    @Test
    void testMissingPluginBacksOffThenRestoresWithoutStoppingListener() {
        try (Fixture fixture = new Fixture()) {
            fixture.failAfterClearingPlugins();
            fixture.admin = new Snapshot("v1", 5L);
            fixture.omitPlugin = true;
            fixture.poll(SERVER);
            assertEquals(1, fixture.listens);
            assertEquals(3, fixture.pluginFetches.size());
            assertNull(fixture.active);

            fixture.setRetryAfter(System.nanoTime() + TimeUnit.MINUTES.toNanos(1));
            fixture.poll(SERVER);
            assertEquals(2, fixture.listens);
            assertEquals(3, fixture.pluginFetches.size());

            fixture.omitPlugin = false;
            fixture.setRetryAfter(System.nanoTime() - 1);
            fixture.poll(SERVER);
            assertEquals("v1", fixture.active);
            assertEquals(4, fixture.pluginFetches.size());
            fixture.poll(SERVER);
            assertEquals(4, fixture.pluginFetches.size());
        }
    }

    private static final class Fixture implements AutoCloseable {

        private final AtomicBoolean running = Objects.requireNonNull(
                (AtomicBoolean) ReflectionTestUtils.getField(HttpSyncDataService.class, "RUNNING"));

        private final Map<?, ?> cache = Objects.requireNonNull(
                (Map<?, ?>) ReflectionTestUtils.getField(AbstractDataRefresh.class, "GROUP_CACHE"));

        private final MetaDataSubscriber metadata = mock(MetaDataSubscriber.class);

        private final List<String> pluginFetches = new ArrayList<>();

        private final HttpSyncDataService service;

        private Snapshot admin = new Snapshot("v1", 5L);

        private String active;

        private String pluginVersion;

        private boolean metadataChanged;

        private boolean failServer;

        private boolean omitPlugin;

        private int listens;

        private Fixture() {
            final PluginDataSubscriber subscriber = new PluginDataSubscriber() {
                @Override
                public void refreshPluginDataAll() {
                    active = null;
                }

                @Override
                public void onSubscribe(final PluginData data) {
                    if ("v2".equals(data.getName())) {
                        throw new IllegalStateException("subscriber rejected v2");
                    }
                    active = data.getName();
                }
            };
            final OkHttpClient client = new OkHttpClient.Builder().addInterceptor(chain -> respond(chain.request())).build();
            HttpConfig config = new HttpConfig();
            config.setUrl(SERVER + "," + HEALTHY_PEER);
            AccessTokenManager tokens = mock(AccessTokenManager.class);
            when(tokens.getAccessToken()).thenReturn("token");
            cache.clear();
            ReflectionTestUtils.setField(PluginDataRefresh.class, "recoveryRequired", false);
            // Drive individual poll turns, without starting a background worker.
            running.set(true);
            service = new HttpSyncDataService(config, subscriber, client, List.of(metadata), List.of(), List.of(),
                    List.of(), List.of(), tokens, new ShenyuConfig());
            fetchPlugin();
        }

        private Response respond(final Request request) {
            String path = request.url().encodedPath();
            String server = request.url().resolve("/").toString();
            server = server.substring(0, server.length() - 1);
            String body;
            int status = 200;
            if (Constants.SHENYU_ADMIN_PATH_CONFIGS_FETCH.equals(path)) {
                List<String> groups = request.url().queryParameterValues("groupKeys");
                boolean plugin = groups.contains(ConfigGroupEnum.PLUGIN.name());
                boolean meta = groups.contains(ConfigGroupEnum.META_DATA.name());
                if (plugin) {
                    pluginFetches.add(server);
                    if (failServer && SERVER.equals(server)) {
                        status = 503;
                    }
                }
                List<String> payload = new ArrayList<>();
                if (plugin && !omitPlugin) {
                    payload.add(admin.pluginJson());
                }
                if (meta) {
                    metadataChanged = false;
                    payload.add("\"META_DATA\":{\"md5\":\"meta-1\",\"lastModifyTime\":1,\"data\":[]}");
                }
                body = "{\"data\":{" + String.join(",", payload) + "}}";
            } else if (Constants.SHENYU_ADMIN_PATH_CONFIGS_LISTENER.equals(path)) {
                listens++;
                pluginVersion = request.url().queryParameter(ConfigGroupEnum.PLUGIN.name());
                List<String> groups = new ArrayList<>();
                if (Objects.isNull(pluginVersion) || !pluginVersion.startsWith(admin.digest() + ",")) {
                    groups.add("\"PLUGIN\"");
                }
                if (metadataChanged) {
                    groups.add("\"META_DATA\"");
                }
                body = "{\"data\":[" + String.join(",", groups) + "]}";
            } else {
                throw new AssertionError("Unexpected path: " + path);
            }
            return new Response.Builder().request(request).protocol(Protocol.HTTP_1_1)
                    .code(status).message("Synthetic Admin response").body(ResponseBody.create(body, null)).build();
        }

        private void failAfterClearingPlugins() {
            assertEquals("v1", active);
            admin = new Snapshot("v2", 6L);
            assertThrows(IllegalStateException.class, this::fetchPlugin);
            assertNull(active);
        }

        private void fetchPlugin() {
            ReflectionTestUtils.invokeMethod(service, "doFetchGroupConfig", SERVER, new ConfigGroupEnum[]{ConfigGroupEnum.PLUGIN});
        }

        private void poll(final String server) {
            ReflectionTestUtils.invokeMethod(service, "doLongPolling", server);
        }

        @SuppressWarnings("unchecked")
        private void setRetryAfter(final long retryAfter) {
            Map<String, Long> schedule = (Map<String, Long>) ReflectionTestUtils.getField(service, "pluginRetryAfter");
            Objects.requireNonNull(schedule).put(SERVER, retryAfter);
        }

        @Override
        public void close() {
            service.close();
            running.set(false);
            cache.clear();
            ReflectionTestUtils.setField(PluginDataRefresh.class, "recoveryRequired", false);
        }
    }

    private record Snapshot(String digest, long version) {

        private String pluginJson() {
            return String.format("\"PLUGIN\":{\"md5\":\"%s\",\"lastModifyTime\":%d,\"data\":[{\"name\":\"%s\"}]}", digest, version, digest);
        }
    }
}
