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

package org.apache.shenyu.agent.api.spi;

import org.apache.shenyu.agent.api.point.ShenyuAgentJoinPoint;
import org.apache.shenyu.agent.api.point.ShenyuAgentJoinPoint.JoinPointBuilder;

import java.util.Collection;
import java.util.stream.Collectors;

/**
 * The type Abstract agent plugin definition.
 */
public abstract class AbstractAgentPluginDefinition implements AgentPluginDefinition {
    
    /**
     * Define join point builder.
     *
     * @return the collection
     */
    protected abstract Collection<JoinPointBuilder> joinPointBuilder();
    
    /**
     * Collector collection.
     *
     * @return the collection
     */
    @Override
    public final Collection<ShenyuAgentJoinPoint> collector() {
        Collection<JoinPointBuilder> joinPointBuilders = joinPointBuilder();
        return joinPointBuilders.stream().map(JoinPointBuilder::install).collect(Collectors.toList());
    }
}
