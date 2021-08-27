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

package org.apache.shenyu.admin.mybatis;

import org.apache.ibatis.cache.CacheKey;
import org.apache.ibatis.executor.Executor;
import org.apache.ibatis.mapping.BoundSql;
import org.apache.ibatis.mapping.MappedStatement;
import org.apache.ibatis.mapping.ParameterMapping;
import org.apache.ibatis.plugin.Interceptor;
import org.apache.ibatis.plugin.Intercepts;
import org.apache.ibatis.plugin.Invocation;
import org.apache.ibatis.plugin.Plugin;
import org.apache.ibatis.plugin.Signature;
import org.apache.ibatis.reflection.MetaObject;
import org.apache.ibatis.session.Configuration;
import org.apache.ibatis.session.ResultHandler;
import org.apache.ibatis.session.RowBounds;
import org.apache.shenyu.admin.config.properties.DataBaseProperties;
import org.apache.shenyu.common.enums.AdminDataSourceEnum;
import org.apache.shenyu.common.utils.ReflectUtils;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.regex.Pattern;


/**
 * The limit syntax of postgreSql conflicts with that of mysql,
 * So use the interceptor to process the SQL syntax here.
 */
@Intercepts({
    @Signature(type = Executor.class, method = "query", args = {MappedStatement.class, Object.class, RowBounds.class, ResultHandler.class}),
    @Signature(type = Executor.class, method = "query", args = {MappedStatement.class, Object.class, RowBounds.class, ResultHandler.class, CacheKey.class, BoundSql.class})
})
public class PostgreSqlQueryInterceptor implements Interceptor {

    private static final String REGEX = "((limit)|(LIMIT))+.*?\\?+.*?\\?";

    private static final String MATCH_REGEX = "(?s).*(((limit)|(LIMIT))+.*?\\?+.*?\\?).*";

    private static final String SQL_REPLACE_CHAR = "limit ? offset ?";

    @Autowired
    private DataBaseProperties dataBaseProperties;

    @Override
    public Object intercept(final Invocation invocation) throws Throwable {
        Object[] args = invocation.getArgs();
        MappedStatement ms = (MappedStatement) args[0];
        Object parameter = args[1];
        RowBounds rowBounds = (RowBounds) args[2];
        ResultHandler resultHandler = (ResultHandler) args[3];
        Executor executor = (Executor) invocation.getTarget();
        CacheKey cacheKey;
        BoundSql boundSql;
        if (args.length == 4) {
            boundSql = getProcessedBoundSql(ms, ms.getBoundSql(parameter));
            cacheKey = executor.createCacheKey(ms, parameter, rowBounds, boundSql);
        } else {
            cacheKey = (CacheKey) args[4];
            boundSql = getProcessedBoundSql(ms, (BoundSql) args[5]);
        }

        return executor.query(ms, parameter, rowBounds, resultHandler, cacheKey, boundSql);
    }

    private BoundSql getProcessedBoundSql(final MappedStatement ms, final BoundSql boundSql) {
        if (!AdminDataSourceEnum.POSTGRESQL.getValue().equals(dataBaseProperties.getType())) {
            return boundSql;
        }

        BoundSql result;

        List<ParameterMapping> parameterMappings = boundSql.getParameterMappings();
        Configuration configuration = ms.getConfiguration();
        Object parameterObject = boundSql.getParameterObject();
        // Postgresql does not support the mysql escape character {`}, so replace it with {"}.
        // Convert SQL statements to lowercase.
        String script = boundSql.getSql().replace("`", "\"").toLowerCase();
        if (!Pattern.matches(MATCH_REGEX, script)) {
            result = new BoundSql(configuration, script, parameterMappings, parameterObject);
            // Resolve MyBatis interceptor plugin foreach parameter invalidation.
            this.copyParam(boundSql, result);
            return result;
        }

        // PostgreSql has a different limit syntax than mysql
        // The postgreSql limit syntax is limit ${pageSize} offset ${offset}.
        // The 'pageSize' and 'offset' positions of the query statement are swapped here.
        int size = parameterMappings.size();
        int offsetIndex = size - 1;
        int pageSizeIndex = size - 2;
        ParameterMapping offset = parameterMappings.get(offsetIndex);
        ParameterMapping pageSize = parameterMappings.get(pageSizeIndex);
        parameterMappings.set(offsetIndex, pageSize);
        parameterMappings.set(pageSizeIndex, offset);

        result = new BoundSql(
                configuration,
                script.replaceAll(REGEX, SQL_REPLACE_CHAR),
                parameterMappings,
                parameterObject
        );
        // Resolve MyBatis interceptor plugin foreach parameter invalidation.
        this.copyParam(boundSql, result);
        return result;
    }

    private void copyParam(final BoundSql oldSql, final BoundSql newSql) {
        if (ReflectUtils.getFieldValue(oldSql, "metaParameters") != null) {
            MetaObject mo = (MetaObject) ReflectUtils.getFieldValue(oldSql, "metaParameters");
            ReflectUtils.setFieldValue(newSql, "metaParameters", mo);
        }

        if (ReflectUtils.getFieldValue(oldSql, "additionalParameters") != null) {
            Map<String, Object> map = (Map<String, Object>) ReflectUtils.getFieldValue(oldSql, "additionalParameters");
            ReflectUtils.setFieldValue(newSql, "additionalParameters", map);
        }
    }

    @Override
    public Object plugin(final Object target) {
        return Plugin.wrap(target, this);
    }

    @Override
    public void setProperties(final Properties properties) {

    }
}
