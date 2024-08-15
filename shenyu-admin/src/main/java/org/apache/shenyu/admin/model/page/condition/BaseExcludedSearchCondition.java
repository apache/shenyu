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

package org.apache.shenyu.admin.model.page.condition;

import org.apache.commons.lang3.StringUtils;

/**
 * ExcludedSearchCondition.
 */
public abstract class BaseExcludedSearchCondition implements SearchCondition {
    
    /**
     * excluded code.
     */
    public static final String EXCLUDED_CODE = "!";
    
    /**
     * excluded keyword.
     */
    private String excluded;
    
    /**
     * build keyword contains excluded.
     */
    public void init() {
        if (StringUtils.isNotBlank(getKeyword())) {
            setExcluded(StringUtils.substringAfter(getKeyword(), EXCLUDED_CODE));
            setKeyword(StringUtils.substringBefore(getKeyword(), EXCLUDED_CODE));
        }
    }
    
    
    /**
     * set keyword.
     *
     * @param keyword keyword
     */
    public abstract void setKeyword(String keyword);
    
    /**
     * get excluded.
     *
     * @return excluded keyword
     */
    public String getExcluded() {
        return excluded;
    }
    
    /**
     * set excluded.
     *
     * @param excluded excluded keyword
     */
    public void setExcluded(final String excluded) {
        this.excluded = excluded;
    }
}
