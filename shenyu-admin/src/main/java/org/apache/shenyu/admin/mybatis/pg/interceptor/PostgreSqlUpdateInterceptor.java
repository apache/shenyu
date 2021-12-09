package org.apache.shenyu.admin.mybatis.pg.interceptor;

import org.apache.ibatis.mapping.MappedStatement;
import org.apache.ibatis.plugin.Interceptor;
import org.apache.ibatis.plugin.Intercepts;
import org.apache.ibatis.plugin.Invocation;
import org.apache.ibatis.plugin.Signature;

import java.util.Properties;
import java.util.concurrent.Executor;

/**
 * the mybatis interceptor for update/insert/delete.
 */
@Intercepts({
        @Signature(type = Executor.class, method = "update", args = {MappedStatement.class, Object.class})
})
public class PostgreSqlUpdateInterceptor implements Interceptor {
    @Override
    public Object intercept(final Invocation invocation) throws Throwable {
        return null;
    }

    @Override
    public Object plugin(final Object target) {
        return null;
    }

    @Override
    public void setProperties(final Properties properties) {

    }
}
