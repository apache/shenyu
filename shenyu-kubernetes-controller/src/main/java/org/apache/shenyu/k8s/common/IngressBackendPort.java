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

package org.apache.shenyu.k8s.common;

import io.kubernetes.client.openapi.models.CoreV1EndpointPort;
import io.kubernetes.client.openapi.models.V1ServiceBackendPort;
import org.apache.commons.lang3.StringUtils;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * The service port selected by an ingress backend, either a port name or a port number.
 */
public final class IngressBackendPort {

    private static final String TCP_PROTOCOL = "TCP";

    private final String name;

    private final Integer number;

    private IngressBackendPort(final String name, final Integer number) {
        this.name = name;
        this.number = number;
    }

    /**
     * Build the port from the service port of an ingress backend.
     *
     * @param servicePort service port of the ingress backend, may be null
     * @return the selected service port, or null if the backend does not select one
     */
    public static IngressBackendPort from(final V1ServiceBackendPort servicePort) {
        if (Objects.isNull(servicePort)) {
            return null;
        }
        if (Objects.nonNull(servicePort.getNumber()) && servicePort.getNumber() > 0) {
            return new IngressBackendPort(null, servicePort.getNumber());
        }
        if (StringUtils.isNotBlank(servicePort.getName())) {
            return new IngressBackendPort(servicePort.getName().trim(), null);
        }
        return null;
    }

    /**
     * Select the endpoint port that serves the service port, the first TCP port is used when no
     * endpoint port matches because a service may map the selected port to a different target port.
     *
     * @param ports endpoint ports of an endpoint subset
     * @param backendPort service port selected by the ingress backend, may be null
     * @return the endpoint port to route to, or null if the subset does not expose a TCP port
     */
    public static CoreV1EndpointPort selectEndpointPort(final List<CoreV1EndpointPort> ports, final IngressBackendPort backendPort) {
        List<CoreV1EndpointPort> tcpPorts = ports.stream()
                .filter(port -> TCP_PROTOCOL.equals(port.getProtocol()))
                .collect(Collectors.toList());
        if (tcpPorts.isEmpty()) {
            return null;
        }
        if (Objects.nonNull(backendPort)) {
            for (CoreV1EndpointPort tcpPort : tcpPorts) {
                if (backendPort.matches(tcpPort)) {
                    return tcpPort;
                }
            }
        }
        return tcpPorts.get(0);
    }

    /**
     * Whether the endpoint port serves this service port.
     *
     * @param endpointPort endpoint port
     * @return true if the endpoint port matches this service port
     */
    public boolean matches(final CoreV1EndpointPort endpointPort) {
        if (Objects.nonNull(number) && number.equals(endpointPort.getPort())) {
            return true;
        }
        return Objects.nonNull(name) && name.equals(endpointPort.getName());
    }

    /**
     * Get the port name.
     *
     * @return port name, null if the port is selected by number
     */
    public String getName() {
        return name;
    }

    /**
     * Get the port number.
     *
     * @return port number, null if the port is selected by name
     */
    public Integer getNumber() {
        return number;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (Objects.isNull(o) || getClass() != o.getClass()) {
            return false;
        }
        IngressBackendPort that = (IngressBackendPort) o;
        return Objects.equals(name, that.name) && Objects.equals(number, that.number);
    }

    @Override
    public int hashCode() {
        return Objects.hash(name, number);
    }

    @Override
    public String toString() {
        return "IngressBackendPort{name='" + name + "', number=" + number + '}';
    }
}
