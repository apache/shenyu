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

package org.apache.shenyu.register.common.subsriber;

import org.apache.shenyu.disruptor.consumer.QueueConsumerFactory;
import org.apache.shenyu.register.common.type.DataTypeParent;

import java.util.HashSet;
import java.util.Set;

/**
 * The type Abstract queue consumer factory.
 */
public abstract class AbstractQueueConsumerFactory<T extends DataTypeParent> implements QueueConsumerFactory<T> {
    
    /**
     * The Subscribers.
     */
    private final Set<ExecutorSubscriber<T>> subscribers = new HashSet<>();
    
    /**
     * Add subscribers abstract queue consumer factory.
     *
     * @param subscriber the subscriber
     * @return the abstract queue consumer factory
     */
    public AbstractQueueConsumerFactory<T> addSubscribers(final ExecutorSubscriber<T> subscriber) {
        subscribers.add(subscriber);
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
}
