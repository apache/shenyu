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

package org.apache.shenyu.plugin.httpclient;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * The test case for loading the trace context propagator of the http protocol, which has to fail
 * softly so that a broken tracing installation cannot stop the http client plugin.
 */
public class AbstractHttpClientPluginTraceContextLoaderTest {

    @Test
    public void testLoadsTheRegisteredPropagator() {
        Assertions.assertInstanceOf(TestTraceContextPropagator.class,
                AbstractHttpClientPlugin.loadTraceContextPropagator(TestTraceContextPropagator::new));
    }

    @Test
    public void testNoPropagatorWhileTracingIsNotInstalled() {
        // no propagator is registered for the http protocol, which is the normal case while the
        // tracing module is not on the classpath of the gateway
        Assertions.assertNull(AbstractHttpClientPlugin.loadTraceContextPropagator(() -> {
            throw new IllegalArgumentException("http name is error");
        }));
    }

    @Test
    public void testNoPropagatorWhileTheImplementationCannotBeLoaded() {
        // the tracing module is on the classpath while one of its dependencies is missing: the
        // implementation class fails while it is initialized, which raises an error rather than an
        // exception, and it must not reach the plugin
        Assertions.assertNull(AbstractHttpClientPlugin.loadTraceContextPropagator(() -> {
            throw new NoClassDefFoundError("io/opentelemetry/api/trace/Tracer");
        }));
        Assertions.assertNull(AbstractHttpClientPlugin.loadTraceContextPropagator(() -> {
            throw new IllegalStateException("load extension resources error");
        }));
    }
}
