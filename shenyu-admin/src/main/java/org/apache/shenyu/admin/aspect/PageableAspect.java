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

import com.github.pagehelper.Page;
import com.github.pagehelper.PageHelper;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.ReflectUtils;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.springframework.stereotype.Component;

import java.util.Objects;

/**
 * Pageable aop aspect.
 */
@Aspect
@Component
public class PageableAspect {
    
    /**
     * Pageable cut.
     */
    @Pointcut("@annotation(org.apache.shenyu.admin.aspect.annotation.Pageable)")
    public void pageableCut() {
    }
    
    /**
     * mapper processing around.
     *
     * @param point point {@link ProceedingJoinPoint}
     * @return result {@link Object}
     */
    @Around("pageableCut()")
    public Object mapperAround(final ProceedingJoinPoint point) {
        // CHECKSTYLE:OFF
        try {
            Object query = point.getArgs()[0];
            PageParameter pageParameter = (PageParameter) ReflectUtils.getFieldValue(query, "pageParameter");
            if (Objects.isNull(pageParameter)) {
                return point.proceed();
            }
            Page<?> page = PageHelper.startPage(pageParameter.getCurrentPage(), pageParameter.getPageSize());
            Object proceed = point.proceed();
            CommonPager<?> commonPager = (CommonPager<?>) proceed;
            PageParameter result = commonPager.getPage();
            result = convert(page, result);
            commonPager.setPage(result);
            return proceed;
        } catch (Throwable throwable) {
            throw new ShenyuException(throwable);
        }
        // CHECKSTYLE:ON
    }
    
    /**
     * convert PageInfo to PageParameter.
     *
     * @param pageInfo the pageInfo.
     * @param pageParameter the pageParameter.
     * @return PageParameter. page parameter
     */
    public PageParameter convert(final Page<?> pageInfo, final PageParameter pageParameter) {
        pageParameter.setCurrentPage(pageInfo.getPageNum());
        pageParameter.setPageSize(pageInfo.getPageSize());
        pageParameter.setTotalPage(pageInfo.getPages());
        pageParameter.setTotalCount((int) pageInfo.getTotal());
        pageParameter.setOffset(pageInfo.getPageSize());
        return pageParameter;
    }
}
