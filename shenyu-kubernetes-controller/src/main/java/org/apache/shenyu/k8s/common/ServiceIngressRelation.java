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

import java.util.Objects;

/**
 * The relation between a service and the ingress that references it, including the service port
 * selected by the ingress backend.
 */
public final class ServiceIngressRelation {

    private final String ingressNamespace;

    private final String ingressName;

    private final IngressBackendPort port;

    /**
     * Constructor of ServiceIngressRelation.
     *
     * @param ingressNamespace ingress namespace
     * @param ingressName ingress name
     * @param port service port selected by the ingress backend, may be null
     */
    public ServiceIngressRelation(final String ingressNamespace, final String ingressName, final IngressBackendPort port) {
        this.ingressNamespace = ingressNamespace;
        this.ingressName = ingressName;
        this.port = port;
    }

    /**
     * Get the ingress namespace.
     *
     * @return ingress namespace
     */
    public String getIngressNamespace() {
        return ingressNamespace;
    }

    /**
     * Get the ingress name.
     *
     * @return ingress name
     */
    public String getIngressName() {
        return ingressName;
    }

    /**
     * Get the service port selected by the ingress backend.
     *
     * @return selected service port, null if the backend does not select one
     */
    public IngressBackendPort getPort() {
        return port;
    }

    /**
     * Whether the relation belongs to the given ingress.
     *
     * @param namespace ingress namespace
     * @param name ingress name
     * @return true if the relation belongs to the ingress
     */
    public boolean isSameIngress(final String namespace, final String name) {
        return Objects.equals(ingressNamespace, namespace) && Objects.equals(ingressName, name);
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (Objects.isNull(o) || getClass() != o.getClass()) {
            return false;
        }
        ServiceIngressRelation that = (ServiceIngressRelation) o;
        return Objects.equals(ingressNamespace, that.ingressNamespace)
                && Objects.equals(ingressName, that.ingressName)
                && Objects.equals(port, that.port);
    }

    @Override
    public int hashCode() {
        return Objects.hash(ingressNamespace, ingressName, port);
    }

    @Override
    public String toString() {
        return "ServiceIngressRelation{ingressNamespace='" + ingressNamespace + "', ingressName='" + ingressName + "', port=" + port + '}';
    }
}
