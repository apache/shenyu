/*
 *     Licensed to the Apache Software Foundation (ASF) under one or more
 *     contributor license agreements.See the NOTICE file distributed with
 *     this work for additional information regarding copyright ownership.
 *     The ASF licenses this file to You under the Apache License, Version 2.0
 *     (the "License"); you may not use this file except in compliance with
 *     the License.You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *     Unless required by applicable law or agreed to in writing, software
 *     distributed under the License is distributed on an "AS IS" BASIS,
 *     WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *     See the License for the specific language governing permissions and
 *     limitations under the License.
 */

import com.google.common.base.Joiner;
import java.util.List;
import java.util.Optional;
import org.apache.curator.framework.CuratorFramework;
import org.apache.curator.framework.CuratorFrameworkFactory;
import org.apache.curator.framework.state.ConnectionState;
import org.apache.curator.retry.RetryNTimes;
import org.apache.zookeeper.CreateMode;
import org.dromara.soul.common.exception.SoulException;
import org.dromara.soul.common.extension.Join;
import org.dromara.soul.common.http.URL;
import org.dromara.soul.register.api.AbstractRegistry;
import org.dromara.soul.register.api.RegisterConst;
import org.dromara.soul.register.api.config.RegistryConfig;

import static org.dromara.soul.register.api.RegisterConst.BASE_URL_PATH_KEY;

/**
 * ZookeeperRegistry
 * zookeeper theRegistry.
 *
 * @author sixh
 */
@Join
public class ZookeeperRegistry extends AbstractRegistry {

    private final CuratorFramework client;

    public ZookeeperRegistry(URL url) {
        super(url);
        String host = url.getHost();
        Integer port = url.getPort();
        List<String> cluster = RegistryConfig.getCluster(url);
        cluster.add(host + RegisterConst.COLONS + port);
        String connectString = Joiner.on(RegisterConst.URL_SPLIT_SYMBOL_KEY).join(cluster);
        CuratorFrameworkFactory.Builder builder = CuratorFrameworkFactory.builder()
                .connectString(connectString)
                .retryPolicy(new RetryNTimes(1, 1000))
                .connectionTimeoutMs(5000);
        client = builder.build();
        client.getConnectionStateListenable().addListener((curatorFramework, connectionState) -> {
            if (connectionState == ConnectionState.RECONNECTED) {
                retry();
            }
        });
        client.start();
    }

    private void create(String path, boolean ephemeral) {
        if (!ephemeral && checkExists(path)) {
            return;
        }
        int i = path.lastIndexOf(BASE_URL_PATH_KEY);
        if (i > 0) {
            create(path.substring(0, i), false);
        }
        if (ephemeral) {
            createEphemeral(path);
        } else {
            createPersistent(path);
        }
    }

    private void createEphemeral(String path) {
        try {
            client.create().withMode(CreateMode.EPHEMERAL).forPath(path);
        } catch (Exception ignored) {
        }
    }

    private void createPersistent(String path) {
        try {
            client.create().withMode(CreateMode.PERSISTENT).forPath(path);
        } catch (Exception ignored) {
        }
    }

    private boolean checkExists(String path) {
        try {
            return client.checkExists().forPath(path) != null;
        } catch (Exception e) {
            return false;
        }
    }

    @Override
    public String urlProtocol() {
        return "zookeeper";
    }

    private String toPath(URL url) {
        String path = url.getPath();
        if (!path.startsWith(BASE_URL_PATH_KEY)) {
            path = BASE_URL_PATH_KEY + path;
        }
        if (!path.endsWith(BASE_URL_PATH_KEY)) {
            path = path + BASE_URL_PATH_KEY;
        }
        return path + url.encode();
    }

    @Override
    public void register(URL url) {
        try {
            String path = toPath(url);
            create(path, isEphemeral(url));
        } catch (Throwable e) {
            throw new SoulException("register failed " + url + "to zookeeper " + getRemoteUrl());
        }
    }

    private boolean isEphemeral(URL url) {
        String parameter = url.getParameter(RegisterConst.EPHEMERAL_KEY);
        return Optional.ofNullable(parameter)
                .map(e -> "1".equals(e) || "true".equals(e))
                .orElse(false);
    }

    @Override
    public void unregister(URL url) {
        try {
            client.delete().forPath(toPath(url));
        } catch (Throwable e) {
            throw new SoulException("unregister failed " + url + "to zookeeper " + getRemoteUrl());
        }
    }
}
