/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * Contributor license agreements.See the NOTICE file distributed with
 * This work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * he License.You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.dromara.soul.dashboard.spring;

import org.dromara.soul.dashboard.listener.DataChangedListener;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationListener;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

/**
 * Event forwarders, which forward the changed events to each ConfigEventListener.
 *
 * @author huangxiaofeng
 * @author xiaoyu
 */
@Component
public class DataChangedEventDispatcher implements ApplicationListener<DataChangedEvent>, InitializingBean {

    private ApplicationContext applicationContext;

    private List<DataChangedListener> listeners;

    public DataChangedEventDispatcher(final ApplicationContext applicationContext) {
        this.applicationContext = applicationContext;
    }

    @Override
    @SuppressWarnings("unchecked")
    public void onApplicationEvent(final DataChangedEvent event) {
        for (DataChangedListener listener : listeners) {
            switch (event.getGroupKey()) {
                case APP_AUTH:
                    break;
                case PLUGIN:
                    break;
                case RULE:
                    break;
                case SELECTOR:
                    break;
                default:
                    throw new IllegalStateException("Unexpected value: " + event.getGroupKey());
            }
        }
    }

    @Override
    public void afterPropertiesSet() {
        Collection<DataChangedListener> listenerBeans = applicationContext.getBeansOfType(DataChangedListener.class).values();
        this.listeners = Collections.unmodifiableList(new ArrayList<>(listenerBeans));
    }

}
