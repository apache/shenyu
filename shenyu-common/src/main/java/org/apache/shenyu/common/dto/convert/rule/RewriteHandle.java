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

package org.apache.shenyu.common.dto.convert.rule;

import java.util.Objects;

/**
 * this is rewrite plugin handle.
 */
public class RewriteHandle {

    /**
     * java regular expression.
     */
    private String regex;

    /**
     * replace string.
     */
    private String replace;

    /**
     * percentage of rewritten traffic.
     */
    private Integer percentage;
    
    /**
     * rewrite the original metadata.
     */
    private Boolean rewriteMetaData;

    /**
     * get regex.
     *
     * @return regex
     */
    public String getRegex() {
        return regex;
    }

    /**
     * set regex.
     *
     * @param regex regex
     */
    public void setRegex(final String regex) {
        this.regex = regex;
    }

    /**
     * get replace.
     *
     * @return replace
     */
    public String getReplace() {
        return replace;
    }

    /**
     * set replace.
     *
     * @param replace replace
     */
    public void setReplace(final String replace) {
        this.replace = replace;
    }

    /**
     * get percentage.
     *
     * @return percentage
     */
    public Integer getPercentage() {
        return percentage;
    }

    /**
     * set percentage.
     *
     * @param percentage percentage
     */
    public void setPercentage(final Integer percentage) {
        this.percentage = percentage;
    }
    
    /**
     * get rewrite meta data status.
     *
     * @return rewrite meta data status
     */
    public Boolean getRewriteMetaData() {
        return rewriteMetaData;
    }
    
    /**
     * set rewrite meta data.
     *
     * @param rewriteMetaData status
     */
    public void setRewriteMetaData(final Boolean rewriteMetaData) {
        this.rewriteMetaData = rewriteMetaData;
    }
    
    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (Objects.isNull(o) || getClass() != o.getClass()) {
            return false;
        }
        RewriteHandle that = (RewriteHandle) o;
        return Objects.equals(regex, that.regex) && Objects.equals(replace, that.replace)
                && Objects.equals(rewriteMetaData, that.rewriteMetaData);
    }

    @Override
    public int hashCode() {
        return Objects.hash(regex, replace, rewriteMetaData);
    }

    @Override
    public String toString() {
        return "RewriteHandle{"
                + "regex='"
                + regex
                + '\''
                + ", replace='"
                + replace
                + '\''
                + ", percentage='"
                + percentage
                + '\''
                + ", rewriteMetaData='"
                + rewriteMetaData
                + '\''
                + '}';
    }
}
