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
 * this is RequestHandle plugin handle.
 */
public class MockHandle {

    private Integer httpStatusCode;

    private String responseContent;

    /**
     * get http status code.
     * @return http status code.
     */
    public Integer getHttpStatusCode() {
        return httpStatusCode;
    }

    /**
     * set http status code.
     *
     * @param httpStatusCode http status code.
     */
    public void setHttpStatusCode(final Integer httpStatusCode) {
        this.httpStatusCode = httpStatusCode;
    }

    /**
     * get response content.
     *
     * @return content.
     */
    public String getResponseContent() {
        return responseContent;
    }

    /**
     * set response content.
     *
     * @param responseContent content.
     */
    public void setResponseContent(final String responseContent) {
        this.responseContent = responseContent;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        MockHandle that = (MockHandle) o;
        return Objects.equals(httpStatusCode, that.httpStatusCode) && Objects.equals(responseContent, that.responseContent);
    }

    @Override
    public int hashCode() {
        return Objects.hash(httpStatusCode, responseContent);
    }

    @Override
    public String toString() {
        return "MockHandle{"
                + "httpStatusCode="
                + httpStatusCode
                + ", responseContent='"
                + responseContent
                + '}';
    }
}
