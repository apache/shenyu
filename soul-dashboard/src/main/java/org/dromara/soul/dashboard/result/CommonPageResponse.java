/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * Contributor license agreements.See the NOTICE file distributed with
 * This work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * he License.You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.dromara.soul.dashboard.result;

import lombok.Getter;

import java.io.Serializable;
import java.util.Collections;
import java.util.List;

/**
 * @param <T> the type parameter
 * @author chenbin xiaoyu
 */
@Getter
public class CommonPageResponse<T> extends CommonResponse<List<T>> implements Serializable {

    /**
     * currentPage.
     */
    private Integer currentPage = 0;

    /**
     * totalCount.
     */
    private long totalCount = 0;

    /**
     * pageSize.
     */
    private Integer pageSize = 0;

    /**
     * Sets current page.
     *
     * @param currentPage the current page
     * @return the current page
     */
    public CommonPageResponse<T> setCurrentPage(Integer currentPage) {
        this.currentPage = currentPage;
        return this;
    }

    /**
     * Sets total count.
     *
     * @param totalCount the total count
     * @return the total count
     */
    public CommonPageResponse<T> setTotalCount(long totalCount) {
        this.totalCount = totalCount;
        return this;
    }

    /**
     * Sets page size.
     *
     * @param pageSize the page size
     * @return the page size
     */
    public CommonPageResponse<T> setPageSize(Integer pageSize) {
        this.pageSize = pageSize;
        return this;
    }

    public CommonPageResponse() {
        this(CommonCode.SUCCESS.getCode(), "", null);
    }

    /**
     * init CalvinResponse.
     *
     * @param code code
     * @param msg  msg
     * @param data data
     */
    public CommonPageResponse(int code, String msg, List<T> data) {
        super(code, msg, data);
    }

    /**
     * CalvinPageResponse.
     *
     * @param currentPage current page.
     * @param pageSize    page size.
     * @param totalCount  totalCount.
     * @param data        data.
     */
    public CommonPageResponse(int currentPage, int pageSize, long totalCount, List<T> data) {
        super(CommonCode.SUCCESS.getCode(), "", data);
        this.currentPage = currentPage;
        this.pageSize = pageSize;
        this.totalCount = totalCount;
    }

    /**
     * Instantiates a new Calvin page response.
     *
     * @param t the t
     */
    public CommonPageResponse(List<T> t) {
        super(CommonCode.SUCCESS.getCode(), "", t);
    }

    /**
     * 静态方法构建一个Page返回对象.
     *
     * @param <T>         data type.
     * @param currentPage current page.
     * @param pageSize    page size.
     * @param totalCount  totalCount.
     * @param data        data.
     * @return CalvinPageResponse calvin page response
     */
    public static <T> CommonPageResponse<T> success(int currentPage, int pageSize, long totalCount, List<T> data) {
        return new CommonPageResponse<>(data)
                .setCurrentPage(currentPage)
                .setPageSize(pageSize)
                .setTotalCount(totalCount);
    }

    /**
     * Success calvin page response.
     * set method {@link #setCurrentPage(Integer)}
     * set method{@link #setPageSize(Integer)}
     * set method{@link #setTotalCount(long)}
     *
     * @param <T>  the type parameter
     * @param data the data
     * @return the calvin page response
     */
    public static <T> CommonPageResponse<T> success(List<T> data) {
        return new CommonPageResponse<>(data);
    }


    public static <T> CommonPageResponse<T> pageError(String msg) {
        return pageError(CommonCode.FAILURE.getCode(), msg);
    }

    public static <T> CommonPageResponse<T> pageError(int code, String msg) {
        return new CommonPageResponse<>(code, msg, null);
    }

    /**
     * Empty calvin page response.
     *
     * @param <T> the type parameter
     * @return the calvin page response
     */
    public static <T> CommonPageResponse<T> empty() {
        return new CommonPageResponse<>(Collections.emptyList());
    }

    @Override
    public String toString() {
        return "CalvinPageResponse{"
                +
                "code=" + getCode()
                +
                ", msg=" + getMsg()
                +
                ", currentPage=" + currentPage
                +
                ", totalCount=" + totalCount
                +
                ", pageSize=" + pageSize
                +
                ", data=" + getData()
                +
                '}';
    }
}
