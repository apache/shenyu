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

package org.dromara.soul.admin.page;

import java.util.Collections;
import java.util.List;
import java.util.function.Supplier;

/**
 * The type Page result utils.
 *
 * @author xiaoyu
 */
public class PageResultUtils {

    /**
     * Result common pager.
     *
     * @param <T>           the type parameter
     * @param pageParameter the page parameter
     * @param count         the count
     * @param supplier      the supplier
     * @return the common pager
     */
    public static <T> CommonPager<T> result(final PageParameter pageParameter, final Integer count, final Supplier<List<T>> supplier) {
        if (count != null && count > 0) {
            return new CommonPager<>(new PageParameter(pageParameter.getCurrentPage(), pageParameter.getPageSize(), count), supplier.get());
        }
        return new CommonPager<>(new PageParameter(pageParameter.getCurrentPage(), pageParameter.getPageSize(), 0), Collections.emptyList());

    }
}
