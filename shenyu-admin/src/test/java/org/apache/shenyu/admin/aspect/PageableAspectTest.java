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
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.MetaDataQuery;
import org.apache.shenyu.common.exception.ShenyuException;
import org.aspectj.lang.ProceedingJoinPoint;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Test cases for {@link PageableAspect}.
 */
@ExtendWith(MockitoExtension.class)
public class PageableAspectTest {

    @InjectMocks
    private PageableAspect pageableAspect;

    @Mock
    private ProceedingJoinPoint point;

    @Test
    public void testMapperAround() {
        MetaDataQuery metaDataQuery = new MetaDataQuery();
        PageParameter pageParameter = new PageParameter();
        MetaDataQuery[] metaDataQueries = {metaDataQuery};
        /**
         * test null return.
         */
        when(point.getArgs()).thenReturn(metaDataQueries);
        Object result = pageableAspect.mapperAround(point);
        assertTrue(Objects.isNull(result));
        /**
         * test thrown Exception.
         */
        metaDataQuery.setPageParameter(pageParameter);
        when(point.getArgs()).thenReturn(metaDataQueries);
        boolean thrown = false;
        try {
            pageableAspect.mapperAround(point);
        } catch (ShenyuException e) {
            thrown = true;
        }
        assertTrue(thrown);
        /**
         * test return proceed.
         */
        CommonPager commonPager = mock(CommonPager.class);
        metaDataQuery.setPageParameter(pageParameter);
        when(point.getArgs()).thenReturn(metaDataQueries);
        pageParameter.setCurrentPage(1);
        pageParameter.setPageSize(50);
        pageParameter.setOffset(50);
        try {
            when(point.proceed()).thenReturn(commonPager);
        } catch (Throwable throwable) {
            throw new ShenyuException(throwable);
        }
        PageParameter newPageParameter = new PageParameter();
        when(commonPager.getPage()).thenReturn(newPageParameter);
        CommonPager resultCommonPager = (CommonPager) pageableAspect.mapperAround(point);
        assertEquals(pageParameter, newPageParameter);
        assertEquals(pageParameter, resultCommonPager.getPage());
    }

    @Test
    public void testConvert() {
        final PageParameter pageParameter = new PageParameter();
        Page page = new Page();
        page.setPageNum(1);
        page.setPageSize(50);
        page.setPages(20);
        page.setTotal(1000);
        pageableAspect.convert(page, pageParameter);
        assertTrue(page.getPageNum() == pageParameter.getCurrentPage());
        assertTrue(page.getPageSize() == pageParameter.getPageSize());
        assertTrue(page.getPages() == pageParameter.getTotalPage());
        assertTrue(page.getTotal() == pageParameter.getTotalCount());
        assertTrue(page.getPageSize() == pageParameter.getOffset());
    }

    @Test
    public void pageableCutTest() {
        assertDoesNotThrow(() -> pageableAspect.pageableCut());
    }
}
