package org.dromara.soul.client.springmvc.interceptor;

import org.springframework.web.servlet.handler.HandlerInterceptorAdapter;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * The type Param interceptor.
 *
 * @author xiaoyu
 */
public class ParamInterceptor extends HandlerInterceptorAdapter {

    @Override
    public boolean preHandle(final HttpServletRequest request, final HttpServletResponse response, final Object handler) throws Exception {
        return super.preHandle(request, response, handler);
    }

    @Override
    public void afterCompletion(final HttpServletRequest request, final HttpServletResponse response, final Object handler, final Exception ex) throws Exception {
        super.afterCompletion(request, response, handler, ex);
    }
}
