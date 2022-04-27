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

package org.apache.shenyu.register.client.server.consul;

import com.ecwid.consul.v1.kv.model.GetValue;
import org.springframework.context.ApplicationEvent;

import java.util.Map;

/**
 * The type Consul config changed event.
 */
public class ConsulConfigChangedEvent extends ApplicationEvent {

    private static final long serialVersionUID = 6918871848189317069L;

    private final long consulIndex;

    private final Map<String, GetValue> metadataMap;
    
    /**
     * Instantiates a new consul config changed event.
     *
     * @param source the source
     * @param consulIndex the index
     * @param metadataMap the metadata map
     */
    public ConsulConfigChangedEvent(final Object source, final long consulIndex, final Map<String, GetValue> metadataMap) {
        super(source);
        this.consulIndex = consulIndex;
        this.metadataMap = metadataMap;
    }

    @Override
    public Object getSource() {
        return super.getSource();
    }
    
    /**
     * Get Config index.
     *
     * @return the index
     */
    public long getConsulIndex() {
        return consulIndex;
    }
    
    /**
     * Get Metadata map.
     *
     * @return the metadata map
     */
    public Map<String, GetValue> getMetadataMap() {
        return metadataMap;
    }

}
