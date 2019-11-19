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
 * ClassMetadata
 *
 * @author sixh
 */
public interface ClassMetadata {

    /**
     * Gets class name.
     *
     * @return the class name
     */
    String getClassName();

    /**
     * Is interface boolean.
     *
     * @return the boolean
     */
    boolean isInterface();

    /**
     * Is annotation boolean.
     *
     * @return the boolean
     */
    boolean isAnnotation();

    /**
     * Is abstract boolean.
     *
     * @return the boolean
     */
    boolean isAbstract();

    /**
     * Is concrete boolean.
     *
     * @return the boolean
     */
    boolean isConcrete();

    /**
     * Is final boolean.
     *
     * @return the boolean
     */
    boolean isFinal();

    /**
     * Has super class boolean.
     *
     * @return the boolean
     */
    boolean hasSuperClass();

    /**
     * Gets super class.
     *
     * @return the super class
     */
    String getSuperClass();

    /**
     * Get interface names string [ ].
     *
     * @return the string [ ]
     */
    String[] getInterfaceNames();

    /**
     * Is independent boolean.
     *
     * @return the boolean
     */
    boolean isIndependent();

    /**
     * Has enclosing class boolean.
     *
     * @return the boolean
     */
    boolean hasEnclosingClass();

    /**
     * Gets enclosing class name.
     *
     * @return the enclosing class name
     */
    String getEnclosingClassName();
}
