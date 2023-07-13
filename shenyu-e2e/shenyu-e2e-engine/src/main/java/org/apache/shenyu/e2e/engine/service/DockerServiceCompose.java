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
import org.apache.shenyu.e2e.client.ExternalServiceClient;
import org.apache.shenyu.e2e.client.admin.AdminClient;
import org.apache.shenyu.e2e.client.gateway.GatewayClient;
import org.apache.shenyu.e2e.common.TableView;
import org.apache.shenyu.e2e.engine.config.ShenYuEngineConfigure.DockerConfigure;
import org.apache.shenyu.e2e.engine.config.ShenYuEngineConfigure.DockerConfigure.DockerServiceConfigure;
import org.apache.shenyu.e2e.engine.handler.DataSyncHandler;
import org.apache.shenyu.e2e.engine.service.docker.DockerComposeFile;
import org.apache.shenyu.e2e.engine.service.docker.ShenYuLogConsumer;
import org.junit.jupiter.api.Assertions;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.ResourceUtils;
import org.testcontainers.containers.ContainerState;
import org.testcontainers.containers.DockerComposeContainer;
import org.testcontainers.shaded.org.yaml.snakeyaml.DumperOptions;
import org.testcontainers.shaded.org.yaml.snakeyaml.Yaml;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

/**
 * Start docker environment.
 */
public class DockerServiceCompose implements ServiceCompose {

    private static final Logger log = LoggerFactory.getLogger(DockerServiceCompose.class);

    private static final String GATEWAY_YML_LOCATION = "classpath:./bootstrap-application.yml";

    private static final String ADMIN_YML_LOCATION = "classpath:./admin-application.yml";

    private final DockerComposeContainer<?> container;

    private final DockerConfigure configure;
    
    private final DockerServiceConfigure adminConfigure;

    private final DockerServiceConfigure gatewayConfigure;

    private List<DockerServiceConfigure> externalServiceConfigurations;

    public DockerServiceCompose(final DockerConfigure configure) {
        this.configure = configure;
        this.adminConfigure = configure.getAdmin();
        this.gatewayConfigure = configure.getGateway();
        modifyGatewayConfiguration(this.gatewayConfigure);
        DataSyncHandler.init();
        chooseDataSyn(GATEWAY_YML_LOCATION, this.gatewayConfigure);
        chooseDataSyn(ADMIN_YML_LOCATION, this.adminConfigure);
        DockerComposeFile parsedDockerComposeFile = DockerComposeFile.parse(configure.getDockerComposeFile());
        container = new DockerComposeContainer<>("e2e", parsedDockerComposeFile.getFile());
        List<String> services = parsedDockerComposeFile.getServices();
        services.forEach(name -> container.withLogConsumer(name, new ShenYuLogConsumer(name)));
    }
    
    /**
     * start.
     */
    @Override
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
                for (final String binding : bindings) {
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
    
    private String getBaseUrlByService(final DockerServiceConfigure configure) {
        return configure.getSchema() + "://"
                + container.getServiceHost(configure.getServiceName(), configure.getPort())
                + ":"
                + container.getServicePort(configure.getServiceName(), configure.getPort());
    }
    
    @Override
    public GatewayClient newGatewayClient(final String scenarioId) {
        return new GatewayClient(scenarioId, getGatewayBaseUrl(), gatewayConfigure.getProperties());
    }
    
    @Override
    public AdminClient newAdminClient(final String scenarioId) {
        return new AdminClient(scenarioId, getAdminBaseUrl(), adminConfigure.getProperties());
    }
    
    @Override
    public ExternalServiceClient newExternalServiceClient(final String externalServiceName) {
        DockerServiceConfigure dockerServiceConfigure = configure.getExternalServices().stream()
                .filter(e -> externalServiceName.equals(e.getServiceName()))
                .findFirst()
                .orElseThrow(() -> new AssertionFailedError("ExternalServiceClient[" + externalServiceName + "] configure: not found"));
        String url = getBaseUrlByService(dockerServiceConfigure);
        return new ExternalServiceClient(url, dockerServiceConfigure.getProperties());
    }
    
    /**
     * stop.
     */
    @Override
    public void stop() {
        container.stop();
    }

    /**
     * Modify the application. yml file of the gateway.
     *
     * @param gatewayConfigure gatewayConfigure
     */
    private void modifyGatewayConfiguration(final DockerServiceConfigure gatewayConfigure) {
        String value = gatewayConfigure.getProperties().getProperty("application");
        if (Objects.isNull(value)) {
            return;
        }
        try {
            final File file = Assertions.assertDoesNotThrow(
                () -> ResourceUtils.getFile(GATEWAY_YML_LOCATION)
            );
            final InputStream inputStream = Assertions.assertDoesNotThrow(
                () -> new FileInputStream(file)
            );
            Yaml yaml = new Yaml();
            Map<String, Object> yamlData = yaml.load(inputStream);
            String[] sonValues = value.split(",");
            for (final String sonValue : sonValues) {
                String[] subModule = sonValue.split(":");

                String[] subModulePath = subModule[0].split("\\.");

                modifyYamlValue(yamlData, subModulePath, subModule[1]);
            }

            DumperOptions options = new DumperOptions();
            options.setDefaultFlowStyle(DumperOptions.FlowStyle.BLOCK);
            options.setExplicitStart(true);
            options.setDefaultScalarStyle(DumperOptions.ScalarStyle.PLAIN);
            yaml = new Yaml(options);
            final OutputStream outputStream = Assertions.assertDoesNotThrow(
                () -> new FileOutputStream(file)
            );
            yaml.dump(yamlData, new OutputStreamWriter(outputStream));
        } catch (Exception e) {
            log.error(e.getMessage(), e);
        }
    }

    @SuppressWarnings("unchecked")
    private void modifyYamlValue(final Map<String, Object> yamlData, final String[] subModulePath, final String newValue) {
        if (subModulePath.length == 0) {
            return;
        }

        Map<String, Object> currentMap = yamlData;
        for (int i = 0; i < subModulePath.length - 1; i++) {
            if (!currentMap.containsKey(subModulePath[i])) {
                currentMap.put(subModulePath[i], new LinkedHashMap<>());
            }
            currentMap = (Map<String, Object>) currentMap.get(subModulePath[i]);
        }

        currentMap.put(subModulePath[subModulePath.length - 1], newValue);
    }

    /**
     * Modify the data synchronization method in the application. yml file of gateway or admin.
     *
     * @param path path
     * @param dockerServiceConfigure dockerServiceConfigure
     */
    @SuppressWarnings("unchecked")
    private void chooseDataSyn(final String path, final DockerServiceConfigure dockerServiceConfigure) {
        String value = dockerServiceConfigure.getProperties().getProperty("dataSyn");
        if (Objects.isNull(value)) {
            return;
        }
        try {
            final File file = Assertions.assertDoesNotThrow(
                () -> ResourceUtils.getFile(path)
            );
            final InputStream inputStream = Assertions.assertDoesNotThrow(
                () -> new FileInputStream(file)
            );
            Yaml yaml = new Yaml();
            Map<String, Object> yamlData = yaml.load(inputStream);
            Map<String, Object> shenyuParameter = (Map<String, Object>) yamlData.get("shenyu");
            Map<String, Object> parameter = (Map<String, Object>) shenyuParameter.get("sync");
            Map<String, Object> subParameters = DataSyncHandler.getDataSynMap(dockerServiceConfigure.getProperties().getProperty("dataSyn"));
            String synMethod = "";
            if (dockerServiceConfigure.getProperties().getProperty("dataSyn").contains("_")) {
                synMethod = dockerServiceConfigure.getProperties().getProperty("dataSyn").split("_")[1];
            } else {
                synMethod = dockerServiceConfigure.getProperties().getProperty("dataSyn");
            }
            parameter.put(synMethod, subParameters);
            String finalSynMethod = synMethod;
            parameter.keySet().removeIf(key -> !key.equals(finalSynMethod));
            DumperOptions options = new DumperOptions();
            options.setDefaultFlowStyle(DumperOptions.FlowStyle.BLOCK);
            options.setExplicitStart(true);
            options.setDefaultScalarStyle(DumperOptions.ScalarStyle.PLAIN);
            yaml = new Yaml(options);
            final OutputStream outputStream = Assertions.assertDoesNotThrow(
                () -> new FileOutputStream(file)
            );
            yaml.dump(yamlData, new OutputStreamWriter(outputStream));
        } catch (Exception e) {
            log.error(e.getMessage(), e);
        }
    }
}
