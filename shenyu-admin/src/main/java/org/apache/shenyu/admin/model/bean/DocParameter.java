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

import com.google.gson.annotations.SerializedName;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import org.apache.commons.lang3.StringUtils;

/**
 * DocParameter.
 * Parameter, type, required or not, maximum length, description, example value.
 */
public class DocParameter {

    private static final AtomicInteger GEN = new AtomicInteger();

    private Integer id = GEN.incrementAndGet();

    private String module;

    private String name;

    private String type;

    private String maxLength = "-";

    private boolean required;

    private String description;

    private String example = "";

    @SerializedName("x-example")
    private String xExample;

    private List<DocParameter> refs;

    /**
     * getId.
     *
     * @return Integer
     */
    public Integer getId() {
        return id;
    }

    /**
     * setId.
     * @param id id
     */
    public void setId(final Integer id) {
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
     * getType.
     *
     * @return String
     */
    public String getType() {
        return type;
    }

    /**
     * setType.
     *
     * @param type type
     */
    public void setType(final String type) {
        this.type = type;
    }

    /**
     * getMaxLength.
     *
     * @return String
     */
    public String getMaxLength() {
        return maxLength;
    }

    /**
     * setMaxLength.
     *
     * @param maxLength maxLength
     */
    public void setMaxLength(final String maxLength) {
        this.maxLength = maxLength;
    }

    /**
     * isRequired.
     *
     * @return boolean
     */
    public boolean isRequired() {
        return required;
    }

    /**
     * setRequired.
     *
     * @param required required
     */
    public void setRequired(final boolean required) {
        this.required = required;
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
     * get xExample.
     *
     * @return String
     */
    public String getXExample() {
        return xExample;
    }

    /**
     * set xExample.
     * @param xExample xExample
     */
    public void setXExample(final String xExample) {
        this.xExample = xExample;
    }

    /**
     * getExample.
     *
     * @return String
     */
    public String getExample() {
        return StringUtils.isBlank(example) ? xExample : example;
    }

    /**
     * setExample.
     *
     * @param example example
     */
    public void setExample(final String example) {
        this.example = example;
    }

    /**
     * getRefs.
     *
     * @return List
     */
    public List<DocParameter> getRefs() {
        return refs;
    }

    /**
     *  setRefs.
     *
     * @param refs refs
     */
    public void setRefs(final List<DocParameter> refs) {
        this.refs = refs;
    }
}
