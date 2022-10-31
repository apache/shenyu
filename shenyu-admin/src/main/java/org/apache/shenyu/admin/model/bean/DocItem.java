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

package org.apache.shenyu.admin.model.bean;

import java.util.Collection;
import java.util.List;
import org.apache.commons.lang3.StringUtils;

/**
 * DocItem.
 */
public class DocItem {

    private String id;

    private String module;

    private String name;

    private String summary = StringUtils.EMPTY;

    private String description = StringUtils.EMPTY;

    /**
     * Whether to upload multiple files.
     */
    private boolean multiple;

    /**
     * http method list.
     */
    private Collection<String> httpMethodList;

    private Collection<String> produces;

    /**
     * Module Order.
     */
    private int moduleOrder;

    /**
     * api doc Order.
     */
    private int apiOrder;

    private List<DocParameter> requestHeaders;

    private List<DocParameter> requestParameters;

    private List<DocParameter> responseParameters;

    private List<CustomCode> bizCodeList;

    /**
     * isUploadRequest.
     * Whether it is a file upload request.
     *
     * @return boolean
     */
    public boolean isUploadRequest() {
        boolean upload = false;
        if (requestParameters != null) {
            for (DocParameter requestParameter : requestParameters) {
                String type = requestParameter.getType();
                if ("file".equalsIgnoreCase(type)) {
                    upload = true;
                    break;
                }
            }
        }
        return multiple || upload;
    }

    /**
     * getId.
     *
     * @return String
     */
    public String getId() {
        return id;
    }

    /**
     * setId.
     *
     * @param id id
     */
    public void setId(final String id) {
        this.id = id;
    }

    /**
     * getModule.
     *
     * @return String
     */
    public String getModule() {
        return module;
    }

    /**
     * setModule.
     *
     * @param module module
     */
    public void setModule(final String module) {
        this.module = module;
    }

    /**
     * getName.
     *
     * @return String
     */
    public String getName() {
        return name;
    }

    /**
     * setName.
     *
     * @param name name
     */
    public void setName(final String name) {
        this.name = name;
    }

    /**
     * getSummary.
     *
     * @return String
     */
    public String getSummary() {
        return summary;
    }

    /**
     * setSummary.
     *
     * @param summary summary
     */
    public void setSummary(final String summary) {
        this.summary = summary;
    }

    /**
     * getDescription.
     *
     * @return String
     */
    public String getDescription() {
        return description;
    }

    /**
     * setDescription.
     *
     * @param description description
     */
    public void setDescription(final String description) {
        this.description = description;
    }

    /**
     * isMultiple.
     *
     * @return String
     */
    public boolean isMultiple() {
        return multiple;
    }

    /**
     * setMultiple.
     *
     * @param multiple multiple
     */
    public void setMultiple(final boolean multiple) {
        this.multiple = multiple;
    }

    /**
     * getHttpMethodList.
     *
     * @return String
     */
    public Collection<String> getHttpMethodList() {
        return httpMethodList;
    }

    /**
     * setHttpMethodList.
     *
     * @param httpMethodList httpMethodList
     */
    public void setHttpMethodList(final Collection<String> httpMethodList) {
        this.httpMethodList = httpMethodList;
    }

    /**
     * getProduces.
     *
     * @return String
     */
    public Collection<String> getProduces() {
        return produces;
    }

    /**
     * setProduces.
     *
     * @param produces produces
     */
    public void setProduces(final Collection<String> produces) {
        this.produces = produces;
    }

    /**
     * getModuleOrder.
     *
     * @return String
     */
    public int getModuleOrder() {
        return moduleOrder;
    }

    /**
     * setModuleOrder.
     *
     * @param moduleOrder moduleOrder
     */
    public void setModuleOrder(final int moduleOrder) {
        this.moduleOrder = moduleOrder;
    }

    /**
     * getApiOrder.
     *
     * @return String
     */
    public int getApiOrder() {
        return apiOrder;
    }

    /**
     * setApiOrder.
     *
     * @param apiOrder apiOrder
     */
    public void setApiOrder(final int apiOrder) {
        this.apiOrder = apiOrder;
    }

    /**
     * getRequestHeaders.
     * @return request headers
     */
    public List<DocParameter> getRequestHeaders() {
        return requestHeaders;
    }

    /**
     * setRequestHeaders.
     *
     * @param requestHeaders requestHeaders
     */
    public void setRequestHeaders(final List<DocParameter> requestHeaders) {
        this.requestHeaders = requestHeaders;
    }

    /**
     * getRequestParameters.
     *
     * @return List
     */
    public List<DocParameter> getRequestParameters() {
        return requestParameters;
    }

    /**
     * setRequestParameters.
     *
     * @param requestParameters requestParameters
     */
    public void setRequestParameters(final List<DocParameter> requestParameters) {
        this.requestParameters = requestParameters;
    }

    /**
     * getResponseParameters.
     *
     * @return List
     */
    public List<DocParameter> getResponseParameters() {
        return responseParameters;
    }

    /**
     * setResponseParameters.
     *
     * @param responseParameters responseParameters
     */
    public void setResponseParameters(final List<DocParameter> responseParameters) {
        this.responseParameters = responseParameters;
    }

    /**
     * getBizCodeList.
     *
     * @return List
     */
    public List<CustomCode> getBizCodeList() {
        return bizCodeList;
    }

    /**
     * setBizCodeList.
     *
     * @param bizCodeList bizCodeList
     */
    public void setBizCodeList(final List<CustomCode> bizCodeList) {
        this.bizCodeList = bizCodeList;
    }
}
