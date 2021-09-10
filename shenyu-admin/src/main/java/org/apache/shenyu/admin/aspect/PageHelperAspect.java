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

package org.apache.shenyu.admin.aspect;

import com.github.pagehelper.PageHelper;
import com.github.pagehelper.PageInfo;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.utils.ThreadLocalUtil;
import org.apache.shenyu.common.exception.CommonErrorCode;
import org.apache.shenyu.common.exception.ShenyuException;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.springframework.stereotype.Component;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import javax.servlet.http.HttpServletRequest;
import java.util.List;

/**
 * PageHelper aop aspect.
 */
@Aspect
@Component
public class PageHelperAspect {

    /**
     * define pageHelper mapper aop point cut.
     */
    @Pointcut("@annotation(org.apache.shenyu.admin.aspect.annotation.PageHelperMapperAnnotation)")
    public void pageHelperMapperCut() {
    }

    /**
     * define pageHelper controller aop point cut.
     */
    @Pointcut("@annotation(org.apache.shenyu.admin.aspect.annotation.PageHelperControllerAnnotation)")
    public void pageHelperControllerCut() {
    }


    /**
     * mapper processing around.
     *
     * @param point point {@link ProceedingJoinPoint}
     * @return result {@link Object}
     */
    @Around("pageHelperMapperCut()")
    public Object mapperAround(final ProceedingJoinPoint point) {
        // CHECKSTYLE:OFF
        try {
            ServletRequestAttributes attributes = (ServletRequestAttributes) RequestContextHolder.getRequestAttributes();
            HttpServletRequest request = attributes.getRequest();
            Integer currentPage = Integer.valueOf(request.getParameter("currentPage"));
            Integer pageSize = Integer.valueOf(request.getParameter("pageSize"));

            PageHelper.startPage(currentPage, pageSize);

            Object proceed = point.proceed();
            List data = (List) proceed;
            ThreadLocalUtil.put("page", new PageInfo<>(data));
            return proceed;
        } catch (Throwable throwable) {
            throw new ShenyuException(throwable);
        }
        // CHECKSTYLE:ON
    }

    /**
     * controller processing around.
     *
     * @param point point {@link ProceedingJoinPoint}
     * @return result {@link Object}
     */
    @Around("pageHelperControllerCut()")
    public Object controllerAround(final ProceedingJoinPoint point) {
        // CHECKSTYLE:OFF
        try {
            ShenyuAdminResult result = (ShenyuAdminResult) point.proceed();
            if (result.getCode() == CommonErrorCode.SUCCESSFUL) {
                CommonPager commonPager = (CommonPager) result.getData();
                PageParameter pageParameter = commonPager.getPage();
                if (pageParameter.getTotalCount() < 1) {
                    return result;
                }

                PageInfo pageInfo = (PageInfo) ThreadLocalUtil.get("page");
                pageParameter = convertPageInfo2PageParameter(pageInfo, pageParameter);
                commonPager.setPage(pageParameter);
                commonPager.setDataList(pageInfo.getList());
                result.setData(commonPager);
            }
            return result;
        } catch (Throwable throwable) {
            throw new ShenyuException(throwable);
        }
        // CHECKSTYLE:ON
    }

    /**
     * convert PageInfo to PageParameter.
     *
     * @param pageInfo      the pageInfo.
     * @param pageParameter the pageParameter.
     * @return PageParameter.
     */
    public PageParameter convertPageInfo2PageParameter(final PageInfo pageInfo, final PageParameter pageParameter) {
        pageParameter.setCurrentPage(pageInfo.getPageNum());
        pageParameter.setPageSize(pageInfo.getPageSize());
        pageParameter.setPrePage(pageInfo.getPrePage());
        pageParameter.setNextPage(pageInfo.getNextPage());
        pageParameter.setTotalPage(pageInfo.getPages());
        pageParameter.setTotalCount((int) pageInfo.getTotal());
        pageParameter.setOffset(pageInfo.getPageSize());
        return pageParameter;
    }

}
