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

import org.dromara.soul.common.exception.SoulException;
import org.dromara.soul.common.extension.Join;
import org.dromara.soul.common.http.URL;
import org.dromara.soul.register.api.FailbackRegistry;
import org.dromara.soul.register.api.RegisterConst;
import org.dromara.soul.remoting.zookeeper.ZookeeperClient;
import org.dromara.soul.remoting.zookeeper.ZookeeperClientCache;
import org.dromara.soul.remoting.zookeeper.ZookeeperStatusCallback;

import java.util.Optional;

import static org.dromara.soul.register.api.RegisterConst.BASE_URL_PATH_KEY;

/**
 * ZookeeperRegistry
 * zookeeper theRegistry.
 *
 * @author sixh
 */
@Join
public class ZookeeperRegistry extends FailbackRegistry {

    private final ZookeeperClient client;

    public ZookeeperRegistry(URL url) {
        super(url);
        client = ZookeeperClientCache.getClient(url);
        client.statusChange(connectionState -> {
            if (connectionState.equals(ZookeeperStatusCallback.RECONNECTED)) {
                recover();
            }
        });
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


    private boolean isEphemeral(URL url) {
        String parameter = url.getParameter(RegisterConst.EPHEMERAL_KEY);
        return Optional.ofNullable(parameter)
                .map(e -> "1".equals(e) || "true".equals(e))
                .orElse(false);
    }

    @Override
    protected void doRegister(URL url) {
        try {
            String path = toPath(url);
            client.create(path, isEphemeral(url));
        } catch (Throwable e) {
            throw new SoulException("register failed " + url + "to zookeeper " + getRemoteUrl());
        }
    }

    @Override
    protected void doUnRegister(URL url) {
        try {
            client.delete(toPath(url));
        } catch (Throwable e) {
            throw new SoulException("unregister failed " + url + "to zookeeper " + getRemoteUrl());
        }
    }

    @Override
    protected void doSubscribe(URL url) {

    }

    @Override
    protected void doUnSubscribe(URL url) {

    }
}
