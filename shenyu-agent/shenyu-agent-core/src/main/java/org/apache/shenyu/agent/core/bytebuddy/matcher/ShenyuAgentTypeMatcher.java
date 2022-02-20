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

package org.apache.shenyu.agent.core.bytebuddy.matcher;

import net.bytebuddy.description.type.TypeDefinition;
import net.bytebuddy.description.type.TypeDescription;
import net.bytebuddy.matcher.ElementMatcher;
import org.apache.shenyu.agent.api.point.ShenyuAgentJoinPoint;
import org.apache.shenyu.agent.core.loader.ShenyuAgentPluginLoader;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Collections;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.ReentrantLock;

/**
 * The type Shenyu agent type matcher.
 */
public final class ShenyuAgentTypeMatcher extends ElementMatcher.Junction.AbstractBase<TypeDefinition> {
    
    private static final ShenyuAgentTypeMatcher SHENYU_AGENT_TYPE_MATCHER = new ShenyuAgentTypeMatcher();
    
    private static final Logger LOG = LoggerFactory.getLogger(ShenyuAgentTypeMatcher.class);
    
    private final ConcurrentHashMap<String, Object> objectPool = new ConcurrentHashMap<>();
    
    private final ReentrantLock lock = new ReentrantLock();
    
    private Map<String, ShenyuAgentJoinPoint> joinPointMap;
    
    private ShenyuAgentTypeMatcher() {
    }
    
    /**
     * Gets instance.
     *
     * @return the instance
     */
    public static ShenyuAgentTypeMatcher getInstance() {
        return SHENYU_AGENT_TYPE_MATCHER;
    }

    @Override
    public boolean matches(final TypeDefinition target) {
        return joinPointMap.containsKey(target.getTypeName());
    }
    
    /**
     * Contains type boolean.
     *
     * @param target the target
     * @return the boolean
     */
    public boolean containsType(final TypeDescription target) {
        return joinPointMap.containsKey(target.getTypeName());
    }
    
    /**
     * Load shenyu agent join point shenyu agent join point.
     *
     * @param typeDescription the type description
     * @return the shenyu agent join point
     */
    public ShenyuAgentJoinPoint loadShenyuAgentJoinPoint(final TypeDescription typeDescription) {
        return joinPointMap.getOrDefault(typeDescription.getTypeName(),
                new ShenyuAgentJoinPoint("", Collections.emptyList(), Collections.emptyList(), Collections.emptyList()));
    }
    
    /**
     * Gets or create instance.
     *
     * @param <T> the type parameter
     * @param className the handler class name
     * @return the or create instance
     */
    @SuppressWarnings("unchecked")
    public <T> T getOrCreateInstance(final String className) {
        if (objectPool.containsKey(className)) {
            return (T) objectPool.get(className);
        }
        lock.lock();
        try {
            Object inst = objectPool.get(className);
            if (Objects.isNull(inst)) {
                try {
                    inst = Class.forName(className, true, ShenyuAgentPluginLoader.getInstance()).newInstance();
                } catch (InstantiationException | IllegalAccessException | ClassNotFoundException ex) {
                    LOG.error("can not class for name to {}, exception is {} ", className, ex.getMessage());
                    return null;
                }
                objectPool.put(className, inst);
            }
            return (T) inst;
        } finally {
            lock.unlock();
        }
    }
    
    /**
     * Gets join point map.
     *
     * @return the join point map
     */
    public Map<String, ShenyuAgentJoinPoint> getJoinPointMap() {
        return joinPointMap;
    }
    
    /**
     * Sets join point map.
     *
     * @param joinPointMap the join point map
     */
    public void setJoinPointMap(final Map<String, ShenyuAgentJoinPoint> joinPointMap) {
        this.joinPointMap = joinPointMap;
    }
}

