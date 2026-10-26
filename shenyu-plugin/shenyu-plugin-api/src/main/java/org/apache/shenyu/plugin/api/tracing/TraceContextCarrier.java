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

package org.apache.shenyu.plugin.api.tracing;

/**
 * The carrier of an outgoing call, which a {@link TraceContextPropagator} writes the trace context
 * into.
 *
 * <p>Every protocol provides its own carrier, for example the http client plugin hands the headers
 * of the outgoing request over, so that a propagator does not have to know which protocol it is
 * called for.</p>
 */
public interface TraceContextCarrier {

    /**
     * Sets a carrier entry, replacing the values which the entry already has.
     *
     * @param name the name of the entry
     * @param value the value of the entry
     */
    void set(String name, String value);

    /**
     * Removes a carrier entry.
     *
     * @param name the name of the entry
     */
    void remove(String name);
}
