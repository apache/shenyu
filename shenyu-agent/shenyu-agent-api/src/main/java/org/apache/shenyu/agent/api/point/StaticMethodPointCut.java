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
 *
 */

package org.apache.shenyu.agent.api.point;

import net.bytebuddy.description.method.MethodDescription;
import net.bytebuddy.matcher.ElementMatcher;

/**
 * The type Class static method point cut.
 */
public final class StaticMethodPointCut {
    
    private final ElementMatcher<? super MethodDescription> matcher;
    
    private final String handler;
    
    /**
     * Instantiates a new Class static method point cut.
     *
     * @param matcher the matcher
     * @param handler the handler
     */
    public StaticMethodPointCut(final ElementMatcher<? super MethodDescription> matcher, final String handler) {
        this.matcher = matcher;
        this.handler = handler;
    }
    
    /**
     * Gets matcher.
     *
     * @return the matcher
     */
    public ElementMatcher<? super MethodDescription> getMatcher() {
        return matcher;
    }
    
    /**
     * Gets handler.
     *
     * @return the handler
     */
    public String getHandler() {
        return handler;
    }
}
