/*
 * Copyright 2012-2018 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.dromara.config.api;

import com.google.common.collect.Lists;

import java.util.List;
import java.util.Map;

/**
 * {@link OriginLookup} backed by a {@link Map} containing {@link OriginTrackedValue
 * OriginTrackedValues}.
 *
 * @author Madhura Bhave
 * @author Phillip Webb
 * @see OriginTrackedValue
 * @since 2.0.0
 */
public final class TrackedMapPropertySource extends PropertySource<Map<String, Object>> {


    public TrackedMapPropertySource(String name, Map<String, Object> source) {
        super(name, source);
    }

    @Override
    public Object getProperty(String propertySourceName) {
        return this.source.get(propertySourceName);
    }

    @Override
    public List<String> getPropertyKeys() {
        return Lists.newArrayList(this.source.keySet());
    }
}
