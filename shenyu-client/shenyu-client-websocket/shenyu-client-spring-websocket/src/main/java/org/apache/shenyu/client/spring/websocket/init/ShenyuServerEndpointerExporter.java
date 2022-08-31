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

package org.apache.shenyu.client.spring.websocket.init;

import org.apache.shenyu.client.spring.websocket.annotation.ShenyuServerEndpoint;
import org.apache.shenyu.common.exception.ShenyuException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.core.annotation.AnnotatedElementUtils;
import org.springframework.lang.Nullable;
import org.springframework.util.Assert;
import org.springframework.web.context.support.WebApplicationObjectSupport;

import javax.servlet.ServletContext;
import javax.websocket.DeploymentException;
import javax.websocket.server.ServerContainer;
import javax.websocket.server.ServerEndpointConfig;
import java.util.Arrays;

public class ShenyuServerEndpointerExporter extends WebApplicationObjectSupport {

    private static final Logger LOG = LoggerFactory.getLogger(ShenyuServerEndpointerExporter.class);

    @Nullable
    private ServerContainer serverContainer;

    public ShenyuServerEndpointerExporter() {
    }

    /**
     * Set server container.
     *
     * @param serverContainer not null
     */
    public void setServerContainer(@Nullable final ServerContainer serverContainer) {
        this.serverContainer = serverContainer;
    }

    @Nullable
    protected ServerContainer getServerContainer() {
        return this.serverContainer;
    }

    protected void initServletContext(final ServletContext servletContext) {
        if (this.serverContainer == null) {
            this.serverContainer = (ServerContainer) servletContext.getAttribute("javax.websocket.server.ServerContainer");
        }
    }

    protected boolean isContextRequired() {
        return false;
    }

    /**
     * Register endpoint.
     * @param pojo pojo
     */
    public void registerEndpoint(final Class<?> pojo) {
        ShenyuServerEndpoint annotation = AnnotatedElementUtils.findMergedAnnotation(pojo, ShenyuServerEndpoint.class);
        if (annotation == null) {
            throw new ShenyuException("Class missing annotation ShenyuServerEndpoint! class name: " + pojo.getName());
        }

        String path = annotation.value();
        Class<? extends ServerEndpointConfig.Configurator> configuratorClazz = annotation.configurator();
        ServerEndpointConfig.Configurator configurator = null;
        if (!configuratorClazz.equals(ServerEndpointConfig.Configurator.class)) {
            try {
                configurator = (ServerEndpointConfig.Configurator) annotation.configurator().getConstructor().newInstance();
            } catch (ReflectiveOperationException ex) {
                LOG.error("ShenyuServerEndpoint configurator init fail! Class name: {}, configurator name: {}", pojo.getClass().getName(), annotation.configurator().getName());
                throw new ShenyuException(ex);
            }
        }
        ServerEndpointConfig sec = ServerEndpointConfig.Builder.create(pojo, path)
                .decoders(Arrays.asList(annotation.decoders()))
                .encoders(Arrays.asList(annotation.encoders()))
                .subprotocols(Arrays.asList(annotation.subprotocols()))
                .configurator(configurator).build();
        this.registerEndpoint(sec);
    }

    private void registerEndpoint(final ServerEndpointConfig endpointConfig) {
        ServerContainer serverContainer = this.getServerContainer();
        Assert.state(serverContainer != null, "No ServerContainer set");

        try {
            if (this.logger.isDebugEnabled()) {
                this.logger.debug("Registering ServerEndpointConfig: " + endpointConfig);
            }

            serverContainer.addEndpoint(endpointConfig);
        } catch (DeploymentException ex) {
            throw new IllegalStateException("Failed to register ServerEndpointConfig: " + endpointConfig, ex);
        }
    }
}
