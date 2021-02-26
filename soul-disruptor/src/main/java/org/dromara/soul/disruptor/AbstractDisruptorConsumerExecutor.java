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

package org.dromara.soul.disruptor;

import java.util.HashSet;
import java.util.Set;

/**
 * DisruptorConsumerExecutor.
 * disruptor consumer executor.
 *
 * @param <T> the type parameter
 */
public abstract class AbstractDisruptorConsumerExecutor<T> {

    /**
     * Recorded the subscription processing after the user needs to subscribe to the calculation result.
     */
    private Set<ExecutorSubscriber<T>> subscribers = new HashSet<>();

    /**
     * Add subscribers disruptor consumer executor.
     *
     * @param subscriber subscriber；
     * @return the disruptor consumer executor
     */
    public AbstractDisruptorConsumerExecutor<T> addSubscribers(final ExecutorSubscriber<T> subscriber) {
        subscribers.add(subscriber);
        return this;
    }

    /**
     * Add subscribers disruptor consumer executor.
     *
     * @param subscribers the subscribers
     * @return the disruptor consumer executor
     */
    public AbstractDisruptorConsumerExecutor<T> addSubscribers(final Set<ExecutorSubscriber<T>> subscribers) {
        subscribers.forEach(this::addSubscribers);
        return this;
    }

    /**
     * Gets subscribers.
     *
     * @return the subscribers
     */
    public Set<ExecutorSubscriber<T>> getSubscribers() {
        return subscribers;
    }

    /**
     * Perform the processing of the current event.
     *
     * @param data the data
     */
    public abstract void executor(T data);
}
