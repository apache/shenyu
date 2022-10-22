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

package org.apache.shenyu.admin.mybatis.oracle;

import com.google.common.collect.ImmutableList;
import org.apache.ibatis.executor.Executor;
import org.apache.ibatis.mapping.MappedStatement;
import org.apache.ibatis.plugin.Interceptor;
import org.apache.ibatis.plugin.Intercepts;
import org.apache.ibatis.plugin.Invocation;
import org.apache.ibatis.plugin.Plugin;
import org.apache.ibatis.plugin.Signature;
import org.apache.shenyu.common.utils.ReflectUtils;

import java.lang.reflect.Field;
import java.sql.Timestamp;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.Properties;

/**
 * The 'date_updated' field in the postgreSql library is not automatically updated.
 * So this interceptor intercepts the UPDATE statement.
 * Adds the current time to the 'date_updated' field at run time.
 */
@Intercepts({
    @Signature(type = Executor.class, method = "update", args = {MappedStatement.class, Object.class})
})
public class OracleSQLUpdateInterceptor implements Interceptor {

    private static final List<String> AUTOMATIC_DATES = ImmutableList.of("dateUpdated");

    @Override
    public Object intercept(final Invocation invocation) throws Throwable {
        Object[] args = invocation.getArgs();
        MappedStatement ms = (MappedStatement) args[0];
        Object parameter = args[1];
        Executor executor = (Executor) invocation.getTarget();
        for (Class<?> superClass = parameter.getClass(); superClass != Object.class; superClass = superClass.getSuperclass()) {
            Arrays.stream(superClass.getDeclaredFields())
                    .filter(f -> matchParam(parameter, f))
                    .forEach(f -> ReflectUtils.setFieldValue(parameter, f.getName(), new Timestamp(System.currentTimeMillis())));
        }
        return executor.update(ms, parameter);
    }

    private boolean matchParam(final Object parameter, final Field f) {
        return AUTOMATIC_DATES.contains(f.getName()) && Objects.isNull(ReflectUtils.getFieldValue(parameter, f));
    }

    @Override
    public Object plugin(final Object target) {
        return Plugin.wrap(target, this);
    }

    @Override
    public void setProperties(final Properties properties) {

    }
}
