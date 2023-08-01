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

import org.apache.ibatis.executor.statement.StatementHandler;
import org.apache.ibatis.mapping.BoundSql;
import org.apache.ibatis.plugin.Interceptor;
import org.apache.ibatis.plugin.Intercepts;
import org.apache.ibatis.plugin.Invocation;
import org.apache.ibatis.plugin.Plugin;
import org.apache.ibatis.plugin.Signature;

import java.lang.reflect.Field;
import java.sql.Connection;
import java.util.Properties;

/**
 * the mybatis interceptor for update/insert/delete.
 */
@Intercepts({
        @Signature(type = StatementHandler.class, method = "prepare", args = {Connection.class, Integer.class})
})
public class OracleSQLPrepareInterceptor implements Interceptor {

    @Override
    public Object intercept(final Invocation invocation) throws Throwable {
        StatementHandler statementHandler = (StatementHandler) invocation.getTarget();

        //Elegant access to object properties through MetaObject, here is access to the properties of statementHandler;
        //MetaObject is an object provided by Mybatis for easy and elegant access to object properties,
        // through which you can simplify the code, no need to try/catch various reflect exceptions,
        // while it supports the operation of JavaBean, Collection, Map three types of object operations.
        // MetaObject metaObject = MetaObject
        //        .forObject(statementHandler, SystemMetaObject.DEFAULT_OBJECT_FACTORY, SystemMetaObject.DEFAULT_OBJECT_WRAPPER_FACTORY,
        //                new DefaultReflectorFactory());
        //First intercept to RoutingStatementHandler, there is a StatementHandler type delegate variable,
        // its implementation class is BaseStatementHandler, and then to the BaseStatementHandler member variable mappedStatement

        // MappedStatement mappedStatement = (MappedStatement) metaObject.getValue("delegate.mappedStatement");
        // String id = mappedStatement.getId(); mapper method full path
        // String sqlCommandType = mappedStatement.getSqlCommandType().toString();  sql method eg: insert update delete select
        BoundSql boundSql = statementHandler.getBoundSql();
        // get original sql file
        // reflect modify sql file
        Field field = boundSql.getClass().getDeclaredField("sql");
        field.setAccessible(true);
        // replace desc and true
        String replaceSql = boundSql.getSql().toLowerCase()
                .replace("`desc`", "\"desc\"")
                .replace("true", "'true'");
        // replace`
        replaceSql = replaceSql.replace("`", "");
        // replace resource
        if (replaceSql.contains("resource")) {
            replaceSql = replaceSql.replace("into resource", "into \"resource\"")
                    .replace("from resource", "from \"resource\"")
                    .replace("update resource", "update \"resource\"");
        }
        // replace batch insert into
        if (replaceSql.contains("insert into") && replaceSql.split("\\(").length > 3) {
            replaceSql = replaceSql.replaceAll("\r|\n|\\s", "")
                    .replace("insertinto", "insert into ")
                    .replace("values", " SELECT * FROM (")
                    .replace("(?", " SELECT ?")
                    .replace("),", " FROM dual UNION ALL")
                    .replace("?)", " ? FROM dual)");
        }

        // replace limit 1
        if (replaceSql.contains("select")) {
            if (replaceSql.contains("where")) {
                replaceSql = replaceSql.replace("limit 1", "and rownum = 1");
            } else {
                replaceSql = replaceSql.replace("limit 1", "where rownum = 1");
            }
        }
        field.set(boundSql, replaceSql);

        return invocation.proceed();
    }

    @Override
    public Object plugin(final Object target) {
        return Plugin.wrap(target, this);
    }

    @Override
    public void setProperties(final Properties properties) {

    }
}
