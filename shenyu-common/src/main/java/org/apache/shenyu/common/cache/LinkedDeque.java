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

package org.apache.shenyu.common.cache;

import java.lang.reflect.Method;
import java.util.LinkedList;
import java.util.Optional;

/**
 * The only difference between this class and {@link java.util.LinkedList}
 * is that this class has {@link #moveToBack} method.
 */
public class LinkedDeque<E> extends LinkedList<E> {
    
    private static final long serialVersionUID = 4845212252986039116L;
    
    private static Method unlink;
    
    private static Method linkLast;
    
    static {
        for (Method method : LinkedList.class.getDeclaredMethods()) {
            final String methodName = method.getName();
            if ("unlink".equals(methodName)) {
                unlink = method;
                if (!unlink.isAccessible()) {
                    unlink.setAccessible(true);
                }
            } else if ("linkLast".equals(methodName)) {
                linkLast = method;
                if (!linkLast.isAccessible()) {
                    linkLast.setAccessible(true);
                }
            }
        }
    }
    
    /**
     * move the element to back.
     *
     * @param e the element
     */
    public void moveToBack(final E e) {
        if (contains(e) && e != getLast()) {
            Optional.ofNullable(unlink).ifPresent(unlink -> {
                try {
                    unlink.invoke(this, e);
                } catch (Exception ignored) {
                }
            });
            Optional.ofNullable(linkLast).ifPresent(linkLast -> {
                try {
                    linkLast.invoke(this, e);
                } catch (Exception ignored) {
                }
            });
        }
    }
}
