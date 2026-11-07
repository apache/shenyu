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

package org.apache.shenyu.plugin.base.condition.data;

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.utils.DateUtils;
import org.apache.shenyu.common.utils.ReflectUtils;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.spi.Join;
import org.springframework.web.server.ServerWebExchange;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.time.LocalDateTime;
import java.util.Objects;

/**
 * The type Post parameter data.
 * Strings are returned unchanged and date-time values use the format expected by time predicates.
 * Static fields and unsupported values are treated as absent, like missing context fields.
 */
@Join
public class PostParameterData implements ParameterData {
    
    @Override
    public String builder(final String paramName, final ServerWebExchange exchange) {
        ShenyuContext shenyuContext = exchange.getAttribute(Constants.CONTEXT);
        if (Objects.isNull(shenyuContext)) {
            return null;
        }
        Field field = ReflectUtils.getField(shenyuContext.getClass(), paramName);
        if (Objects.isNull(field) || Modifier.isStatic(field.getModifiers())) {
            return null;
        }
        Object value = ReflectUtils.getFieldValue(shenyuContext, field);
        if (value instanceof LocalDateTime) {
            return DateUtils.localDateTimeToString((LocalDateTime) value);
        }
        return value instanceof String ? (String) value : null;
    }
}
