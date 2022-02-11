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

package org.apache.shenyu.agent.api.point;

import net.bytebuddy.description.method.MethodDescription;
import net.bytebuddy.matcher.ElementMatcher;
import net.bytebuddy.matcher.ElementMatchers;

import java.util.ArrayList;
import java.util.List;

/**
 * The type Shenyu agent join point.
 */
public final class ShenyuAgentJoinPoint {
    
    private final String classTarget;
    
    private final List<ConstructorPointCut> constructorPoints;
    
    private final List<InstanceMethodPointCut> instanceMethodPoints;
    
    private final List<StaticMethodPointCut> staticMethodPoints;
    
    /**
     * Instantiates a new Shenyu agent join point.
     *
     * @param classTarget the class target
     * @param constructorPoints the constructor points
     * @param instanceMethodPoints the instance method points
     * @param staticMethodPoints the static method points
     */
    public ShenyuAgentJoinPoint(final String classTarget, final List<ConstructorPointCut> constructorPoints, 
                                final List<InstanceMethodPointCut> instanceMethodPoints,
                                final List<StaticMethodPointCut> staticMethodPoints) {
        this.classTarget = classTarget;
        this.constructorPoints = constructorPoints;
        this.instanceMethodPoints = instanceMethodPoints;
        this.staticMethodPoints = staticMethodPoints;
    }
    
    /**
     * Intercept class join point builder.
     *
     * @param classTarget the class target
     * @return the join point builder
     */
    public static JoinPointBuilder interceptClass(final String classTarget) {
        return new JoinPointBuilder(classTarget);
    }
    
    /**
     * Gets class target.
     *
     * @return the class target
     */
    public String getClassTarget() {
        return classTarget;
    }
    
    /**
     * Gets constructor points.
     *
     * @return the constructor points
     */
    public List<ConstructorPointCut> getConstructorPoints() {
        return constructorPoints;
    }
    
    /**
     * Gets instance method points.
     *
     * @return the instance method points
     */
    public List<InstanceMethodPointCut> getInstanceMethodPoints() {
        return instanceMethodPoints;
    }
    
    /**
     * Gets static method points.
     *
     * @return the static method points
     */
    public List<StaticMethodPointCut> getStaticMethodPoints() {
        return staticMethodPoints;
    }
    
    /**
     * The type Join point builder.
     */
    public static final class JoinPointBuilder {
        
        private final List<ConstructorPointCut> constructorPoints = new ArrayList<>();
        
        private final List<InstanceMethodPointCut> instanceMethodPoints = new ArrayList<>();
        
        private final List<StaticMethodPointCut> classStaticMethodPoints = new ArrayList<>();
        
        private final String classTarget;
    
        /**
         * Instantiates a new Join point builder.
         *
         * @param classTarget the class target
         */
        public JoinPointBuilder(final String classTarget) {
            this.classTarget = classTarget;
        }
    
        /**
         * On constructor constructor point builder.
         *
         * @param matcher the matcher
         * @return the constructor point builder
         */
        public ConstructorPointBuilder onConstructor(final ElementMatcher<? super MethodDescription> matcher) {
            return new ConstructorPointBuilder(this, matcher);
        }
    
        /**
         * Around instance method point builder.
         *
         * @param matcher the matcher
         * @return the instance method point builder
         */
        public InstanceMethodPointBuilder aroundInstanceMethod(final ElementMatcher<? super MethodDescription> matcher) {
            return new InstanceMethodPointBuilder(this, matcher);
        }
    
        /**
         * Around class static method point builder.
         *
         * @param matcher the matcher
         * @return the static method point builder
         */
        public StaticMethodPointBuilder aroundStaticMethod(final ElementMatcher<? super MethodDescription> matcher) {
            return new StaticMethodPointBuilder(this, matcher);
        }
    
        /**
         * Install shenyu agent join point.
         *
         * @return the shenyu agent join point
         */
        public ShenyuAgentJoinPoint install() {
            return new ShenyuAgentJoinPoint(classTarget, constructorPoints, instanceMethodPoints, classStaticMethodPoints);
        }
    
        /**
         * The type Instance method point builder.
         */
        public static final class InstanceMethodPointBuilder {
            
            private final JoinPointBuilder builder;
            
            private List<String> handlers;
            
            private final ElementMatcher<? super MethodDescription> matcher;
            
            private InstanceMethodPointBuilder(final JoinPointBuilder builder, final ElementMatcher<? super MethodDescription> matcher) {
                this.builder = builder;
                this.matcher = matcher;
            }
    
            /**
             * Handlers instance method point builder.
             *
             * @param handlers the handlers
             * @return the instance method point builder
             */
            public InstanceMethodPointBuilder handlers(final List<String> handlers) {
                this.handlers = handlers;
                return this;
            }
    
            /**
             * Build join point builder.
             *
             * @return the join point builder
             */
            public JoinPointBuilder build() {
                builder.instanceMethodPoints.add(new InstanceMethodPointCut(matcher, handlers));
                return builder;
            }
        }
    
        /**
         * The type Static method point builder.
         */
        public static final class StaticMethodPointBuilder {
            
            private final JoinPointBuilder builder;
            
            private List<String> handlers;
            
            private final ElementMatcher<? super MethodDescription> matcher;
            
            private StaticMethodPointBuilder(final JoinPointBuilder builder, final ElementMatcher<? super MethodDescription> matcher) {
                this.builder = builder;
                this.matcher = ElementMatchers.isStatic().and(matcher);
            }
    
            /**
             * Handlers static method point builder.
             *
             * @param handlers the handlers
             * @return the static method point builder
             */
            public StaticMethodPointBuilder handlers(final List<String> handlers) {
                this.handlers = handlers;
                return this;
            }
    
            /**
             * Build join point builder.
             *
             * @return the join point builder
             */
            public JoinPointBuilder build() {
                builder.classStaticMethodPoints.add(new StaticMethodPointCut(matcher, handlers));
                return builder;
            }
        }
    
        /**
         * The type Constructor point builder.
         */
        public static final class ConstructorPointBuilder {
            
            private final JoinPointBuilder builder;
            
            private final ElementMatcher<? super MethodDescription> matcher;

            private List<String> handlers;
            
            private ConstructorPointBuilder(final JoinPointBuilder builder, final ElementMatcher<? super MethodDescription> matcher) {
                this.builder = builder;
                this.matcher = ElementMatchers.isConstructor().and(matcher);
            }
    
            /**
             * Handlers constructor point builder.
             *
             * @param handlers the handlers
             * @return the constructor point builder
             */
            public ConstructorPointBuilder handlers(final List<String> handlers) {
                this.handlers = handlers;
                return this;
            }
    
            /**
             * Build join point builder.
             *
             * @return the join point builder
             */
            public JoinPointBuilder build() {
                builder.constructorPoints.add(new ConstructorPointCut(matcher, handlers));
                return builder;
            }
        }
    }
}
