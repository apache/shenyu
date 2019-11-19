/*
 *     Licensed to the Apache Software Foundation (ASF) under one or more
 *     contributor license agreements.See the NOTICE file distributed with
 *     this work for additional information regarding copyright ownership.
 *     The ASF licenses this file to You under the Apache License, Version 2.0
 *     (the "License"); you may not use this file except in compliance with
 *     the License.You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *     Unless required by applicable law or agreed to in writing, software
 *     distributed under the License is distributed on an "AS IS" BASIS,
 *     WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *     See the License for the specific language governing permissions and
 *     limitations under the License.
 */

package org.dromara.soul.common.metadata;

/**
 * MethodMetadata
 *
 * @author sixh
 */
public interface MethodMetadata extends AnnotatedTypeMetadata {
    /**
     * Gets method name.
     *
     * @return the method name
     */
    String getMethodName();

    /**
     * Gets declaring class name.
     *
     * @return the declaring class name
     */
    String getDeclaringClassName();

    /**
     * Gets return type name.
     *
     * @return the return type name
     */
    String getReturnTypeName();

    /**
     * Is abstract boolean.
     *
     * @return the boolean
     */
    boolean isAbstract();

    /**
     * Is static boolean.
     *
     * @return the boolean
     */
    boolean isStatic();

    /**
     * Is final boolean.
     *
     * @return the boolean
     */
    boolean isFinal();

    /**
     * Is overridable boolean.
     *
     * @return the boolean
     */
    boolean isOverridable();
}
