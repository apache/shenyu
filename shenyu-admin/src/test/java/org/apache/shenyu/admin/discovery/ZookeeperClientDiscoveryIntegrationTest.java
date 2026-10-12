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
import org.apache.shenyu.client.core.register.ClientDiscoveryConfigRefreshedEventListener;
import org.apache.shenyu.client.core.register.ClientRegisterConfig;
import org.apache.shenyu.client.core.register.ClientRegisterConfigImpl;
import org.apache.shenyu.client.core.register.InstanceRegisterListener;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.client.http.HttpClientRegisterRepository;
import org.apache.shenyu.register.common.config.ShenyuClientConfig;
import org.apache.shenyu.register.common.config.ShenyuDiscoveryConfig;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.apache.shenyu.registry.api.ShenyuInstanceRegisterRepository;
import org.apache.shenyu.registry.api.entity.InstanceEntity;
import org.apache.shenyu.registry.api.path.InstancePathConstants;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.web.server.LocalServerPort;
import org.springframework.context.support.GenericApplicationContext;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.util.ReflectionTestUtils;

import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.WebSocket;
import java.time.Duration;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CompletionStage;
import java.util.concurrent.CopyOnWriteArrayList;

import static org.awaitility.Awaitility.await;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Registers through the real client HTTP sender, Admin and ZooKeeper.
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

    private final List<GenericApplicationContext> clients = new ArrayList<>();

    private final List<DiscoverySyncData> snapshots = new CopyOnWriteArrayList<>();

    private WebSocket websocket;

    @AfterEach
    public void cleanup() {
        if (Objects.nonNull(websocket)) {
            websocket.abort();
        }
        clients.forEach(this::stopClient);
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
        websocket = HttpClient.newHttpClient().newWebSocketBuilder().header(Constants.SHENYU_NAMESPACE_ID, Constants.SYS_DEFAULT_NAMESPACE_ID)
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
            GenericApplicationContext first = startClient(server, "a", 19195, "/shenyu/discovery/http_example");
            awaitUpstreams("a", List.of("127.0.0.1:19195"));
            assertEquals(InstancePathConstants.buildInstanceParentPath("zk-6525-a"), listenerNode("a"));
            ShenyuDiscoveryConfig config = first.getBean(ShenyuDiscoveryConfig.class);
            assertNull(config.getProps().getProperty("watchPath"));
            ShenyuInstanceRegisterRepository reader = reader();
            assertTrue(reader.serviceExists(config.getRegisterPath()));
            assertTrue(reader.serviceExists("zk-6525-a"));
            assertEquals(1, reader.selectInstances(config.getRegisterPath()).size());
            assertFalse(reader.serviceExists("/shenyu/discovery"));
            assertFalse(reader.serviceExists("/shenyu/discovery/http_example"));
            assertTrue(reader.selectInstances("/shenyu/discovery/http_example").isEmpty());
            final GenericApplicationContext other = startClient(server, "b", 19196, null);
            awaitUpstreams("b", List.of("127.0.0.1:19196"));
            assertEquals(InstancePathConstants.buildInstanceParentPath("zk-6525-b"), listenerNode("b"));
            final GenericApplicationContext second = startClient(server, "a", 19197, null);
            awaitUpstreams("a", List.of("127.0.0.1:19195", "127.0.0.1:19197"));
            assertSyncSnapshot("a", List.of("127.0.0.1:19195", "127.0.0.1:19197"));
            InstanceEntity update = InstanceEntity.builder().appName("zk-6525-a").host("127.0.0.1").port(19197).build();
            update.setStatus(0);
            update.setWeight(80);
            writer(second).persistInstance(update);
            await().atMost(Duration.ofSeconds(15)).untilAsserted(() -> assertEquals(80,
                    jdbcTemplate.queryForObject("SELECT weight FROM discovery_upstream WHERE upstream_url = '127.0.0.1:19197'", Integer.class)));
            assertSyncPublished("a", "127.0.0.1:19197", 80);
            // Discovery configuration registration is drained by the Admin subscriber every two seconds.
            Thread.sleep(3000);
            final GenericApplicationContext otherSecond = startClient(server, "b", 19198, null);
            awaitUpstreams("b", List.of("127.0.0.1:19196", "127.0.0.1:19198"));
            awaitUpstreams("a", List.of("127.0.0.1:19195", "127.0.0.1:19197"));
            stopClient(otherSecond);
            awaitUpstreams("b", List.of("127.0.0.1:19196"));
            stopClient(second);
            awaitUpstreams("a", List.of("127.0.0.1:19195"));
            assertSyncSnapshot("a", List.of("127.0.0.1:19195"));
            awaitUpstreams("b", List.of("127.0.0.1:19196"));
            stopClient(first);
            awaitUpstreams("a", List.of());
            assertSyncSnapshot("a", List.of());
            awaitUpstreams("b", List.of("127.0.0.1:19196"));
            stopClient(other);
            awaitUpstreams("b", List.of());
            registeredDiscoveries().forEach(discovery -> processorHolder.chooseProcessor("zookeeper").removeDiscovery(discovery));
        }
    }

    private GenericApplicationContext startClient(final TestingServer server, final String app, final int port, final String registerPath) {
        GenericApplicationContext context = new GenericApplicationContext();
        clients.add(context);
        ShenyuClientConfig client = new ShenyuClientConfig();
        client.setNamespace(Constants.SYS_DEFAULT_NAMESPACE_ID);
        ShenyuClientConfig.ClientPropertiesConfig properties = new ShenyuClientConfig.ClientPropertiesConfig();
        properties.getProps().setProperty("contextPath", "/zk-6525-" + app);
        properties.getProps().setProperty("appName", "zk-6525-" + app);
        properties.getProps().setProperty("host", "127.0.0.1");
        properties.getProps().setProperty("port", String.valueOf(port));
        client.getClient().put("http", properties);
        final ClientRegisterConfig register = new ClientRegisterConfigImpl(client, RpcTypeEnum.HTTP, context, context.getEnvironment());
        ShenyuDiscoveryConfig discovery = new ShenyuDiscoveryConfig();
        discovery.setType("zookeeper");
        discovery.setServerList(server.getConnectString());
        discovery.setRegisterPath(registerPath);
        discovery.getProps().setProperty("name", register.getAppName());
        DiscoveryUpstreamData upstream = new DiscoveryUpstreamData();
        upstream.setUrl(register.getHost() + ":" + register.getPort());
        upstream.setProtocol("http://");
        upstream.setStatus(0);
        upstream.setWeight(50);
        ShenyuRegisterCenterConfig admin = new ShenyuRegisterCenterConfig();
        admin.setServerLists("http://127.0.0.1:" + adminPort);
        admin.getProps().setProperty("username", "admin");
        admin.getProps().setProperty("password", "123456");
        HttpClientRegisterRepository sender = new HttpClientRegisterRepository(admin);
        context.registerBean(ShenyuDiscoveryConfig.class, () -> discovery);
        context.registerBean(InstanceRegisterListener.class, () -> new InstanceRegisterListener(upstream, discovery));
        context.registerBean(ClientDiscoveryConfigRefreshedEventListener.class,
                () -> new ClientDiscoveryConfigRefreshedEventListener(discovery, sender, register, PluginEnum.DIVIDE, client));
        context.refresh();
        return context;
    }

    private void stopClient(final GenericApplicationContext context) {
        if (!context.isActive()) {
            return;
        }
        InstanceRegisterListener listener = context.getBean(InstanceRegisterListener.class);
        writer(context).close();
        ReflectionTestUtils.setField(listener, "discoveryService", null);
        context.close();
    }

    private ShenyuInstanceRegisterRepository writer(final GenericApplicationContext context) {
        return (ShenyuInstanceRegisterRepository) ReflectionTestUtils.getField(context.getBean(InstanceRegisterListener.class), "discoveryService");
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
