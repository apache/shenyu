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

import org.apache.shenyu.client.core.register.ApiBean;
import org.apache.shenyu.client.core.register.matcher.Matcher;
import org.apache.shenyu.client.core.register.parser.Parser;
import org.apache.shenyu.client.core.disruptor.ShenyuClientRegisterEventPublisher;
import org.apache.shenyu.register.common.type.DataTypeParent;

import java.util.List;

public class ApiRegistrar<T, D extends DataTypeParent> extends AbstractRegistrar<ApiBean<T>.ApiDefinition> {

    private final ShenyuClientRegisterEventPublisher publisher;

    private final Parser<List<D>, ApiBean<T>.ApiDefinition> parser;

    public ApiRegistrar(final Matcher<ApiBean<T>.ApiDefinition> matcher,
                        final Parser<List<D>, ApiBean<T>.ApiDefinition> parser,
                        final ShenyuClientRegisterEventPublisher publisher) {
        super(matcher);
        this.publisher = publisher;
        this.parser = parser;
    }

    @Override
    protected final void doRegister(final ApiBean<T>.ApiDefinition element) {

        List<? extends D> datas = parser.parse(element);

        datas.forEach(publisher::publishEvent);
    }
}
