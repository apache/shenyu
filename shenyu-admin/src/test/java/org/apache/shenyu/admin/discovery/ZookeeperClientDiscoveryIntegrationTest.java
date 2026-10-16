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

package org.apache.shenyu.admin.discovery;

import com.google.gson.JsonObject;
import jakarta.annotation.Resource;
import org.apache.curator.test.TestingServer;
import org.apache.shenyu.admin.AbstractSpringIntegrationTest;
import org.apache.shenyu.admin.mapper.DiscoveryMapper;
import org.apache.shenyu.admin.model.entity.DiscoveryDO;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.common.protocol.RegistrationProtocol;
import org.apache.shenyu.registry.api.ShenyuInstanceRegisterRepository;
import org.apache.shenyu.registry.api.config.RegisterConfig;
import org.apache.shenyu.registry.api.entity.InstanceEntity;
import org.apache.shenyu.registry.api.path.InstancePathConstants;
import org.apache.shenyu.registry.core.ShenyuInstanceRegisterRepositoryFactory;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.web.server.LocalServerPort;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.test.context.TestPropertySource;

import java.io.InputStream;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.net.http.WebSocket;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.List;
import java.util.Objects;
import java.util.Properties;
import java.util.concurrent.CompletionStage;
import java.util.concurrent.CopyOnWriteArrayList;

import static org.awaitility.Awaitility.await;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Replays the discovery wire contract through Admin and main-owned ZooKeeper APIs.
 */
@TestPropertySource(properties = "shenyu.sync.websocket.token=zk-6525-sync")
public class ZookeeperClientDiscoveryIntegrationTest extends AbstractSpringIntegrationTest {

    @LocalServerPort
    private int adminPort;

    @Resource
    private JdbcTemplate jdbcTemplate;

    @Resource
    private DiscoveryProcessorHolder processorHolder;

    @Resource
    private DiscoveryMapper discoveryMapper;

    private final List<ShenyuInstanceRegisterRepository> clients = new CopyOnWriteArrayList<>();

    private final List<DiscoverySyncData> snapshots = new CopyOnWriteArrayList<>();

    private final HttpClient httpClient = HttpClient.newBuilder().connectTimeout(Duration.ofSeconds(10)).build();

    private WebSocket websocket;

    @AfterEach
    public void cleanup() {
        if (Objects.nonNull(websocket)) {
            websocket.abort();
        }
        clients.forEach(this::stopInstance);
        List<DiscoveryDO> discoveries = registeredDiscoveries();
        discoveries.forEach(discovery -> processorHolder.chooseProcessor("zookeeper").removeDiscovery(discovery));
        jdbcTemplate.update("DELETE FROM discovery_upstream WHERE discovery_handler_id IN "
                + "(SELECT discovery_handler_id FROM discovery_rel WHERE selector_id LIKE 'zk-6525-%')");
        jdbcTemplate.update("DELETE FROM discovery_handler WHERE id IN (SELECT discovery_handler_id FROM discovery_rel WHERE selector_id LIKE 'zk-6525-%')");
        jdbcTemplate.update("DELETE FROM discovery_rel WHERE selector_id LIKE 'zk-6525-%'");
        discoveries.forEach(discovery -> jdbcTemplate.update("DELETE FROM discovery WHERE id = ?", discovery.getId()));
        jdbcTemplate.update("DELETE FROM selector WHERE id LIKE 'zk-6525-%'");
    }

    @Test
    public void testClientRegistrationAndInstanceChanges() throws Exception {
        websocket = httpClient.newWebSocketBuilder().header(Constants.SHENYU_NAMESPACE_ID, Constants.SYS_DEFAULT_NAMESPACE_ID)
                .header(Constants.X_SHENYU_SYNC_TOKEN, "zk-6525-sync")
                .header(Constants.CLIENT_PORT_NAME, "19194")
                .buildAsync(URI.create("ws://127.0.0.1:" + adminPort + "/websocket"), new WebSocket.Listener() {
                    private final StringBuilder message = new StringBuilder();

                    @Override
                    public CompletionStage<?> onText(final WebSocket socket, final CharSequence data, final boolean last) {
                        message.append(data);
                        if (last) {
                            JsonObject frame = GsonUtils.getInstance().fromJson(message.toString(), JsonObject.class);
                            if (ConfigGroupEnum.DISCOVER_UPSTREAM.name().equals(frame.get("groupType").getAsString())) {
                                snapshots.addAll(GsonUtils.getInstance().fromList(frame.get("data").toString(), DiscoverySyncData.class));
                            }
                            message.setLength(0);
                        }
                        socket.request(1);
                        return null;
                    }
                }).join();
        try (TestingServer server = new TestingServer()) {
            createSelector("a");
            createSelector("b");
            final ShenyuInstanceRegisterRepository first = startInstance(server, "a", 19195);
            awaitUpstreams("a", List.of("127.0.0.1:19195"));
            assertEquals(InstancePathConstants.buildInstanceParentPath("zk-6525-a"), listenerNode("a"));
            String registerPath = InstancePathConstants.buildInstanceParentPath("zk-6525-a");
            ShenyuInstanceRegisterRepository reader = reader();
            assertTrue(reader.serviceExists(registerPath));
            assertTrue(reader.serviceExists("zk-6525-a"));
            assertEquals(1, reader.selectInstances(registerPath).size());
            assertFalse(reader.serviceExists("/shenyu/discovery"));
            assertFalse(reader.serviceExists("/shenyu/discovery/http_example"));
            assertTrue(reader.selectInstances("/shenyu/discovery/http_example").isEmpty());
            final ShenyuInstanceRegisterRepository other = startInstance(server, "b", 19196);
            awaitUpstreams("b", List.of("127.0.0.1:19196"));
            assertEquals(InstancePathConstants.buildInstanceParentPath("zk-6525-b"), listenerNode("b"));
            final ShenyuInstanceRegisterRepository second = startInstance(server, "a", 19197);
            awaitUpstreams("a", List.of("127.0.0.1:19195", "127.0.0.1:19197"));
            assertSyncSnapshot("a", List.of("127.0.0.1:19195", "127.0.0.1:19197"));
            InstanceEntity update = InstanceEntity.builder().appName("zk-6525-a").host("127.0.0.1").port(19197).build();
            update.setStatus(0);
            update.setWeight(80);
            second.persistInstance(update);
            await().atMost(Duration.ofSeconds(15)).untilAsserted(() -> assertEquals(80,
                    jdbcTemplate.queryForObject("SELECT weight FROM discovery_upstream WHERE upstream_url = '127.0.0.1:19197'", Integer.class)));
            assertSyncPublished("a", "127.0.0.1:19197", 80);
            // Discovery configuration registration is drained by the Admin subscriber every two seconds.
            Thread.sleep(3000);
            final ShenyuInstanceRegisterRepository otherSecond = startInstance(server, "b", 19198);
            awaitUpstreams("b", List.of("127.0.0.1:19196", "127.0.0.1:19198"));
            awaitUpstreams("a", List.of("127.0.0.1:19195", "127.0.0.1:19197"));
            stopInstance(otherSecond);
            awaitUpstreams("b", List.of("127.0.0.1:19196"));
            stopInstance(second);
            awaitUpstreams("a", List.of("127.0.0.1:19195"));
            assertSyncSnapshot("a", List.of("127.0.0.1:19195"));
            awaitUpstreams("b", List.of("127.0.0.1:19196"));
            stopInstance(first);
            awaitUpstreams("a", List.of());
            assertSyncSnapshot("a", List.of());
            awaitUpstreams("b", List.of("127.0.0.1:19196"));
            stopInstance(other);
            awaitUpstreams("b", List.of());
            registeredDiscoveries().forEach(discovery -> processorHolder.chooseProcessor("zookeeper").removeDiscovery(discovery));
        }
    }

    private ShenyuInstanceRegisterRepository startInstance(final TestingServer server, final String app, final int port) throws Exception {
        String appName = "zk-6525-" + app;
        Properties properties = new Properties();
        properties.setProperty("name", appName);
        RegisterConfig config = new RegisterConfig("zookeeper", server.getConnectString(), properties);
        ShenyuInstanceRegisterRepository repository = ShenyuInstanceRegisterRepositoryFactory.loadRepository("zookeeper");
        clients.add(repository);
        repository.init(config);
        repository.persistInstance(InstanceEntity.builder().appName(appName).host("127.0.0.1").port(port).build());
        JsonObject discovery;
        try (InputStream input = Objects.requireNonNull(getClass().getResourceAsStream("/registration/v1/discovery-config.json"))) {
            discovery = GsonUtils.getInstance().fromJson(new String(input.readAllBytes(), StandardCharsets.UTF_8), JsonObject.class);
        }
        discovery.addProperty("selectorName", "/" + appName);
        discovery.addProperty("serverList", server.getConnectString());
        discovery.addProperty("listenerNode", InstancePathConstants.buildInstanceParentPath(appName));
        discovery.getAsJsonObject("props").addProperty("name", appName);
        String login = post(RegistrationProtocol.LOGIN_PATH, "{\"userName\":\"admin\",\"password\":\"123456\"}", "");
        String token = GsonUtils.getInstance().fromJson(login, JsonObject.class).getAsJsonObject("data").get("token").getAsString();
        assertEquals(RegistrationProtocol.SUCCESS, post(RegistrationProtocol.DISCOVERY_CONFIG_PATH, discovery.toString(), token));
        return repository;
    }

    private String post(final String path, final String json, final String token) throws Exception {
        HttpRequest request = HttpRequest.newBuilder(URI.create("http://127.0.0.1:" + adminPort + path))
                .timeout(Duration.ofSeconds(15))
                .header("Content-Type", "application/json")
                .header(Constants.X_ACCESS_TOKEN, token)
                .POST(HttpRequest.BodyPublishers.ofString(json)).build();
        HttpResponse<String> response = httpClient.send(request, HttpResponse.BodyHandlers.ofString());
        assertEquals(200, response.statusCode(), response.body());
        if (!RegistrationProtocol.LOGIN_PATH.equals(path)) {
            assertEquals(RegistrationProtocol.V1, response.headers().firstValue(RegistrationProtocol.VERSION_HEADER).orElseThrow());
        }
        return response.body();
    }

    private void stopInstance(final ShenyuInstanceRegisterRepository repository) {
        if (clients.remove(repository)) {
            repository.close();
        }
    }

    private ShenyuInstanceRegisterRepository reader() {
        String id = jdbcTemplate.queryForObject("SELECT discovery_id FROM discovery_handler WHERE listener_node = ?", String.class, listenerNode("a"));
        return ((AbstractDiscoveryProcessor) processorHolder.chooseProcessor("zookeeper")).getShenyuDiscoveryService(id);
    }

    private List<DiscoveryDO> registeredDiscoveries() {
        return jdbcTemplate.queryForList("SELECT DISTINCT h.discovery_id FROM discovery_handler h JOIN discovery_rel r ON r.discovery_handler_id = h.id "
                + "WHERE r.selector_id LIKE 'zk-6525-%'", String.class).stream().map(discoveryMapper::selectById).toList();
    }

    private void createSelector(final String app) {
        jdbcTemplate.update("INSERT INTO selector (id, plugin_id, selector_name, match_mode, selector_type, sort_code, "
                + "enabled, loged, continued, match_restful, namespace_id) "
                + "VALUES (?, '5', ?, 0, 0, 0, 1, 0, 0, 0, ?)", "zk-6525-" + app, "/zk-6525-" + app, Constants.SYS_DEFAULT_NAMESPACE_ID);
    }

    private String listenerNode(final String app) {
        return jdbcTemplate.queryForObject("SELECT listener_node FROM discovery_handler h JOIN discovery_rel r ON r.discovery_handler_id = h.id WHERE r.selector_id = ?",
                String.class, "zk-6525-" + app);
    }

    private void awaitUpstreams(final String app, final List<String> expected) {
        await().atMost(Duration.ofSeconds(15)).untilAsserted(() -> assertEquals(expected,
                jdbcTemplate.queryForList("SELECT upstream_url FROM discovery_upstream u JOIN discovery_rel r ON r.discovery_handler_id = u.discovery_handler_id "
                        + "WHERE r.selector_id = ? ORDER BY upstream_url", String.class, "zk-6525-" + app)));
    }

    private void assertSyncPublished(final String app, final String url, final int weight) {
        await().atMost(Duration.ofSeconds(15)).untilAsserted(() -> assertTrue(snapshots.stream()
                .filter(data -> ("zk-6525-" + app).equals(data.getSelectorId()))
                .flatMap(data -> data.getUpstreamDataList().stream()).anyMatch(upstream -> url.equals(upstream.getUrl()) && upstream.getWeight() == weight)));
    }

    private void assertSyncSnapshot(final String app, final List<String> expected) {
        await().atMost(Duration.ofSeconds(15)).untilAsserted(() -> {
            List<DiscoverySyncData> appSnapshots = snapshots.stream()
                    .filter(data -> ("zk-6525-" + app).equals(data.getSelectorId())).toList();
            assertFalse(appSnapshots.isEmpty());
            DiscoverySyncData snapshot = appSnapshots.get(appSnapshots.size() - 1);
            assertEquals(expected, snapshot.getUpstreamDataList().stream().map(DiscoveryUpstreamData::getUrl).sorted().toList());
            assertEquals(Constants.SYS_DEFAULT_NAMESPACE_ID, snapshot.getNamespaceId());
            snapshot.getUpstreamDataList().forEach(upstream -> assertEquals(snapshot.getNamespaceId(), upstream.getNamespaceId()));
        });
    }
}
