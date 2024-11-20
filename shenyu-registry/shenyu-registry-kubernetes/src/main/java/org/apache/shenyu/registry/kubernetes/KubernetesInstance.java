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

package org.apache.shenyu.registry.kubernetes;

import java.net.URI;
import java.util.Map;

/**
 * kubernetes instance.
 */
public class KubernetesInstance {

    private String instanceId;

    private String serviceId;

    private String host;

    private int port;

    private boolean secure;

    private URI uri;

    private Map<String, String> metadata;

    private String scheme;

    private String namespace;

    public KubernetesInstance() {
    }

    public KubernetesInstance(final String instanceId, final String serviceId, final String host,
                              final int port, final boolean secure, final URI uri,
                              final Map<String, String> metadata, final String scheme, final String namespace) {
        this.instanceId = instanceId;
        this.serviceId = serviceId;
        this.host = host;
        this.port = port;
        this.secure = secure;
        this.uri = uri;
        this.metadata = metadata;
        this.scheme = scheme;
        this.namespace = namespace;
    }

    /**
     * get instanceId.
     * @return instanceId.
     */
    public String getInstanceId() {
        return this.instanceId;
    }

    /**
     * get serviceId.
     * @return serviceId.
     */
    public String getServiceId() {
        return this.serviceId;
    }

    /**
     * get host.
     * @return host.
     */
    public String getHost() {
        return this.host;
    }

    /**
     * get port.
     * @return port.
     */
    public int getPort() {
        return this.port;
    }

    /**
     * get secure.
     * @return secure.
     */
    public boolean isSecure() {
        return this.secure;
    }

    /**
     * get uri.
     * @return uri.
     */
    public URI getUri() {
        return this.uri;
    }

    /**
     * get metadata.
     * @return metadata.
     */
    public Map<String, String> getMetadata() {
        return this.metadata;
    }

    /**
     * get scheme.
     * @return scheme.
     */
    public String getScheme() {
        return this.scheme;
    }

    /**
     * get namespace.
     * @return namespace.
     */
    public String getNamespace() {
        return this.namespace;
    }

    /**
     * set instanceId.
     * @param instanceId instance identifier.
     */
    public void setInstanceId(final String instanceId) {
        this.instanceId = instanceId;
    }

    /**
     * set serviceId.
     * @param serviceId service identifier.
     */
    public void setServiceId(final String serviceId) {
        this.serviceId = serviceId;
    }

    /**
     * set host.
     * @param host ipaddress.
     */
    public void setHost(final String host) {
        this.host = host;
    }

    /**
     * set port.
     * @param port port.
     */
    public void setPort(final int port) {
        this.port = port;
    }

    /**
     * set secure.
     * @param secure secure.
     */
    public void setSecure(final boolean secure) {
        this.secure = secure;
    }

    /**
     * set uri.
     * @param uri uri.
     */
    public void setUri(final URI uri) {
        this.uri = uri;
    }

    /**
     * set metadata.
     * @param metadata metadata.
     */
    public void setMetadata(final Map<String, String> metadata) {
        this.metadata = metadata;
    }

    /**
     * set scheme.
     * @param scheme scheme.
     */
    public void setScheme(final String scheme) {
        this.scheme = scheme;
    }

    /**
     * set namespace.
     * @param namespace namespace.
     */
    public void setNamespace(final String namespace) {
        this.namespace = namespace;
    }

}
