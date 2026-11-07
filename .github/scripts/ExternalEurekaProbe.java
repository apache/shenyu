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

import com.sun.net.httpserver.HttpServer;
import org.apache.shenyu.registry.api.ShenyuInstanceRegisterRepository;
import org.apache.shenyu.registry.api.config.RegisterConfig;
import org.apache.shenyu.spi.ExtensionLoader;

import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.concurrent.atomic.AtomicInteger;

/** Tests the packaged external provider with the actual Admin dependency order. */
public final class ExternalEurekaProbe {

    private ExternalEurekaProbe() {
    }

    public static void main(final String[] args) throws Exception {
        HttpServer server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        AtomicInteger requests = new AtomicInteger();
        byte[] applications = """
                {"applications":{"versions__delta":"1","apps__hashcode":"UP_1_","application":[
                  {"name":"CI-PROBE","instance":[{"instanceId":"ci-probe","hostName":"127.0.0.1",
                    "app":"CI-PROBE","ipAddr":"127.0.0.1","status":"UP",
                    "port":{"$":18081,"@enabled":"true"},"vipAddress":"CI-PROBE",
                    "secureVipAddress":"CI-PROBE","metadata":{"weight":"100"},
                    "dataCenterInfo":{"@class":"com.netflix.appinfo.InstanceInfo$DefaultDataCenterInfo","name":"MyOwn"}}]}
                ]}}
                """.getBytes(StandardCharsets.UTF_8);
        server.createContext("/eureka/", exchange -> {
            requests.incrementAndGet();
            exchange.getResponseHeaders().set("Content-Type", "application/json");
            exchange.sendResponseHeaders(200, applications.length);
            try (var body = exchange.getResponseBody()) {
                body.write(applications);
            }
        });
        server.start();
        ShenyuInstanceRegisterRepository repository = null;
        try {
            repository = ExtensionLoader.getExtensionLoader(ShenyuInstanceRegisterRepository.class).getJoin("eureka");
            Path actual = Path.of(repository.getClass().getProtectionDomain().getCodeSource().getLocation().toURI()).toRealPath();
            if (!actual.equals(Path.of(args[0]).toRealPath())) {
                throw new IllegalStateException("Eureka provider did not load from the external runtime: " + actual);
            }
            RegisterConfig config = new RegisterConfig();
            config.setServerLists("http://127.0.0.1:" + server.getAddress().getPort() + "/eureka/");
            repository.init(config);
            if (requests.get() == 0 || repository.selectInstances("CI-PROBE").size() != 1) {
                throw new IllegalStateException("Packaged Eureka provider did not discover the loopback fixture");
            }
            System.out.println("Packaged external Eureka SPI and HTTP discovery probe passed");
        } finally {
            try {
                if (repository != null) {
                    repository.close();
                }
            } finally {
                server.stop(0);
            }
        }
    }
}
