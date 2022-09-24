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

package org.apache.shenyu.e2e.engine.service;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableList.Builder;
import junit.framework.AssertionFailedError;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.apache.shenyu.e2e.client.ExternalServiceClient;
import org.apache.shenyu.e2e.client.admin.AdminClient;
import org.apache.shenyu.e2e.client.gateway.GatewayClient;
import org.apache.shenyu.e2e.common.TableView;
import org.apache.shenyu.e2e.engine.config.ShenYuEngineConfigure.DockerConfigure;
import org.apache.shenyu.e2e.engine.config.ShenYuEngineConfigure.DockerConfigure.DockerServiceConfigure;
import org.apache.shenyu.e2e.engine.service.docker.DockerComposeFile;
import org.apache.shenyu.e2e.engine.service.docker.ShenYuLogConsumer;
import org.testcontainers.containers.ContainerState;
import org.testcontainers.containers.DockerComposeContainer;

import java.util.List;
import java.util.Objects;
import java.util.Optional;

@Slf4j
@Getter
public class DockerServiceCompose implements ServiceCompose {
    private final DockerComposeContainer<?> container;
    private final DockerConfigure configure;
    
    private final DockerServiceConfigure adminConfigure;
    private final DockerServiceConfigure gatewayConfigure;
    
    private List<DockerServiceConfigure> externalServiceConfigurations;
    
    public DockerServiceCompose(DockerConfigure configure) {
        this.configure = configure;
        this.adminConfigure = configure.getAdmin();
        this.gatewayConfigure = configure.getGateway();
        
        DockerComposeFile parsedDockerComposeFile = DockerComposeFile.parse(configure.getDockerComposeFile());
        container = new DockerComposeContainer<>("e2e", parsedDockerComposeFile.getFile());
        
        List<String> services = parsedDockerComposeFile.getServices();
        services.forEach(name -> container.withLogConsumer(name, new ShenYuLogConsumer(name)));
    }
    
    public void start() {
        exposedServices();
        waitingForAvailable();
        
        container.start();
    
        NamingResolver.INSTANCE.ofDockerConfigure(container);
        printServices();
    }
    
    private void exposedServices() {
        Builder<DockerServiceConfigure> builder = ImmutableList.<DockerServiceConfigure>builder()
                .addAll(configure.getExternalServices());
        if (Objects.nonNull(adminConfigure)) {
            builder.add(adminConfigure);
        }
        if (Objects.nonNull(gatewayConfigure)) {
            builder.add(gatewayConfigure);
        }
        externalServiceConfigurations = builder.build();
        
        externalServiceConfigurations.stream()
                .filter(conf -> conf.getPort() > 1024)
                .forEach(conf -> container.withExposedService(conf.getServiceName(), conf.getPort()));
    }
    
    private void waitingForAvailable() {
        if (Objects.nonNull(adminConfigure)) {
            container.waitingFor(
                    adminConfigure.getServiceName(),
                    WaitingForStrategies.newAdminStrategy(adminConfigure.getPort())
            );
        }
        if (Objects.nonNull(gatewayConfigure)) {
            container.waitingFor(
                    gatewayConfigure.getServiceName(),
                    WaitingForStrategies.newGatewayStrategy(gatewayConfigure.getPort())
            );
        }
    
    }
    
    private void printServices() {
        TableView tableView = new TableView("service name", "container port", "mapped host port");
        for (DockerServiceConfigure serviceConfigure : externalServiceConfigurations) {
            Optional<ContainerState> stateOpt = container.getContainerByServiceName(serviceConfigure.getServiceName());
            if (stateOpt.isPresent()) {
                ContainerState state = stateOpt.get();
                List<String> bindings = state.getPortBindings();
                for (String binding : bindings) {
                    tableView.addRow(serviceConfigure.getServiceName(), binding.split(":"));
                }
            }
            
            if (serviceConfigure.getPort() > 1024) {
                Integer hostPort = container.getServicePort(serviceConfigure.getServiceName(), serviceConfigure.getPort());
                tableView.addRow(serviceConfigure.getServiceName(), serviceConfigure.getPort(), hostPort);
            }
        }
        log.info(System.lineSeparator() + tableView.printAsString() + System.lineSeparator());
        
        if (Objects.isNull(adminConfigure) && Objects.isNull(gatewayConfigure)) {
            log.warn("configure of shenyu-admin or shenyu-bootstrap(gateway) has not seen");
        }
    }
    
    private String getAdminBaseUrl() {
        return getBaseUrlByService(adminConfigure);
    }
    
    private String getGatewayBaseUrl() {
        return getBaseUrlByService(gatewayConfigure);
    }
    
    private String getBaseUrlByService(DockerServiceConfigure configure) {
        return configure.getSchema() + "://"
                + container.getServiceHost(configure.getServiceName(), configure.getPort())
                + ":"
                + container.getServicePort(configure.getServiceName(), configure.getPort());
    }
    
    @Override
    public GatewayClient newGatewayClient(String scenarioId) {
        return new GatewayClient(scenarioId, getGatewayBaseUrl(), gatewayConfigure.getProperties());
    }
    
    @Override
    public AdminClient newAdminClient(String scenarioId) {
        return new AdminClient(scenarioId, getAdminBaseUrl(), adminConfigure.getProperties());
    }
    
    @Override
    public ExternalServiceClient newExternalServiceClient(String externalServiceName) {
        DockerServiceConfigure dockerServiceConfigure = configure.getExternalServices().stream()
                .filter(e -> externalServiceName.equals(e.getServiceName()))
                .findFirst()
                .orElseThrow(() -> new AssertionFailedError("ExternalServiceClient[" + externalServiceName + "] configure: not found"));
        String url = getBaseUrlByService(dockerServiceConfigure);
        return new ExternalServiceClient(url, dockerServiceConfigure.getProperties());
    }
    
    public void stop() {
        container.stop();
    }
}
