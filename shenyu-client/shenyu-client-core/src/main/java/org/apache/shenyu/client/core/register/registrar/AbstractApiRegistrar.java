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

package org.apache.shenyu.client.core.register.registrar;

import org.apache.shenyu.client.core.disruptor.ShenyuClientRegisterEventPublisher;
import org.apache.shenyu.client.core.register.ApiBean;
import org.apache.shenyu.register.common.type.DataTypeParent;

import java.util.List;

public abstract class AbstractApiRegistrar<D extends DataTypeParent> implements ApiRegistrar {

    private final ShenyuClientRegisterEventPublisher publisher;

    protected AbstractApiRegistrar(final ShenyuClientRegisterEventPublisher publisher) {
        this.publisher = publisher;
    }

    /**
     * Determines whether the apiBean can be registered in advance.
     *
     * @param apiBean apiBean to be registered
     * @return true or false
     */
    protected Boolean preMatch(final ApiBean apiBean) {
        return false;
    }

    /**
     * Parses the apiBean as a registrable object.
     *
     * @param apiBean apiBean to be parsed
     * @return registrable object
     */
    protected D preParse(final ApiBean apiBean) {
        throw new UnsupportedOperationException("If the preMatch method was implemented ,the preParse method should be implemented.");
    }

    /**
     * Determines whether apiDefinitions of apiBean can be filtered.
     *
     * @param apiBean apiBean to be registered
     * @return true or false
     */
    protected abstract Boolean match(ApiBean apiBean);

    /**
     * Determines whether the apiDefinition can be registered.
     *
     * @param apiDefinition apiDefinition to be registered
     * @return true or false
     */
    protected abstract Boolean match(ApiBean.ApiDefinition apiDefinition);

    /**
     * Parses the apiDefinition as a registrable objects.
     *
     * @param apiDefinition apiDefinition to be parsed
     * @return registrable objects
     */
    protected abstract List<D> parse(ApiBean.ApiDefinition apiDefinition);

    @Override
    public void register(final ApiBean apiBean) {

        if (preMatch(apiBean)) {
            publisher.publishEvent(preParse(apiBean));
            return;
        }

        if (!match(apiBean)) {
            return;
        }

        apiBean.getApiDefinitions().stream()
                .filter(this::match)
                .map(this::parse)
                .forEach(apiDatas -> apiDatas.forEach(publisher::publishEvent));
    }
}
