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

package org.apache.shenyu.web.controller;

import org.springframework.beans.BeansException;
import org.springframework.beans.factory.ObjectProvider;

/**
 * The type Test object provider.
 *
 * @param <R> the type parameter
 */
public class TestObjectProvider<R> implements ObjectProvider<R> {
    
    private final R r;
    
    /**
     * Instantiates a new Test object provider.
     *
     * @param r the r
     */
    public TestObjectProvider(final R r) { 
        this.r = r;
    }
    
    @Override
    public R getObject() throws BeansException {
        return r;
    }
    
    @Override
    public R getObject(final Object... args) throws BeansException {
        return r;
    }
    
    @Override
    public R getIfAvailable() throws BeansException {
        return r;
    }
    
    @Override
    public R getIfUnique() throws BeansException {
        return r;
    }
}
