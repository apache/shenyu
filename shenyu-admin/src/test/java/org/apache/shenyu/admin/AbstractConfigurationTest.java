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

package org.apache.shenyu.admin;

import org.junit.jupiter.api.AfterEach;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.test.context.support.TestPropertySourceUtils;

/**
 * AbstractConfigurationTest for Configuration or Properties.
 */
public abstract class AbstractConfigurationTest {

    private final AnnotationConfigApplicationContext context = new AnnotationConfigApplicationContext();

    /**
     * Get the current mock context.
     *
     * @return AnnotationConfigApplicationContext
     */
    public AnnotationConfigApplicationContext getContext() {
        return context;
    }

    /**
     * clear context.
     */
    @AfterEach
    public void clear() {
        context.close();
    }

    /**
     * Add properties to Environment and register configuration into spring context.
     *
     * @param configuration the configuration class
     * @param inlinedProperties the config properties
     */
    public void load(final Class<?> configuration, final String... inlinedProperties) {
        load(new Class<?>[]{configuration}, inlinedProperties);
    }

    /**
     * Add properties to Environment and register configuration into spring context.
     *
     * @param configuration the configuration class array
     * @param inlinedProperties the config properties
     */
    public void load(final Class<?>[] configuration, final String... inlinedProperties) {
        TestPropertySourceUtils.addInlinedPropertiesToEnvironment(this.context, inlinedProperties);
        this.context.register(configuration);
        this.context.refresh();
    }
}
