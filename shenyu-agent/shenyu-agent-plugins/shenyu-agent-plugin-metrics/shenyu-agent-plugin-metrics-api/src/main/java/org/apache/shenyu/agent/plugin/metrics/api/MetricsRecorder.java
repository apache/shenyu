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

package org.apache.shenyu.agent.plugin.metrics.api;

/**
 * The interface Metrics recorder.
 */
public interface MetricsRecorder {
    
    /**
     * Increment by default.
     */
    default void inc() {
        inc(1.0);
    }
    
    /**
     * Increment  by value.
     *
     * @param value the value
     */
    default void inc(double value) {
    }
    
    /**
     * Increment by labels.
     *
     * @param labels the labels
     */
    default void inc(String... labels) {
        inc(1, labels);
    }
    
    /**
     * Increment by value and labels.
     *
     * @param value the value
     * @param labels the labels
     */
    default void inc(double value, String... labels) {
    }
    
    /**
     * Decrement by default.
     */
    default void dec() {
        dec(1);
    }
    
    /**
     * Decrement by value.
     *
     * @param value the value
     */
    default void dec(double value) {
    }
    
    /**
     * Decrement by labels.
     *
     * @param labels the labels
     */
    default void dec(String... labels) {
        dec(1, labels);
    }
    
    /**
     * Decrement by value and labels.
     *
     * @param value the value
     * @param labels the labels
     */
    default void dec(double value, String... labels) {
    }
    
    /**
     * Observe to histogram and summary.
     *
     * @param value the value
     */
    default void observe(double value) {
    }
}
