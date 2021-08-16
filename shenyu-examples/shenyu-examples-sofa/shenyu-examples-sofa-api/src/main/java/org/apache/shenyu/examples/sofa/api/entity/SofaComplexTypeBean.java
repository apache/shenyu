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

package org.apache.shenyu.examples.sofa.api.entity;

import java.util.List;
import java.util.Map;
import java.util.StringJoiner;

/**
 * Sofa complex type bean.
 */
public class SofaComplexTypeBean {

    private SofaSimpleTypeBean sofaSimpleTypeBean;

    private List<String> idLists;

    private Map<String, String> idMaps;

    public SofaComplexTypeBean() {
    }

    public SofaComplexTypeBean(final SofaSimpleTypeBean sofaSimpleTypeBean, final List<String> idLists, final Map<String, String> idMaps) {
        this.sofaSimpleTypeBean = sofaSimpleTypeBean;
        this.idLists = idLists;
        this.idMaps = idMaps;
    }

    /**
     * Get sofaSimpleTypeBean.
     *
     * @return sofaSimpleTypeBean
     */
    public SofaSimpleTypeBean getSofaSimpleTypeBean() {
        return sofaSimpleTypeBean;
    }

    /**
     * Set sofaSimpleTypeBean.
     *
     * @param sofaSimpleTypeBean sofaSimpleTypeBean
     */
    public void setSofaSimpleTypeBean(final SofaSimpleTypeBean sofaSimpleTypeBean) {
        this.sofaSimpleTypeBean = sofaSimpleTypeBean;
    }

    /**
     * Get idLists.
     *
     * @return idLists
     */
    public List<String> getIdLists() {
        return idLists;
    }

    /**
     * Set idLists.
     *
     * @param idLists idLists
     */
    public void setIdLists(final List<String> idLists) {
        this.idLists = idLists;
    }

    /**
     * Get idMaps.
     *
     * @return idMaps
     */
    public Map<String, String> getIdMaps() {
        return idMaps;
    }

    /**
     * Set idMaps.
     *
     * @param idMaps idMaps
     */
    public void setIdMaps(final Map<String, String> idMaps) {
        this.idMaps = idMaps;
    }

    @Override
    public String toString() {
        return new StringJoiner(", ", SofaComplexTypeBean.class.getSimpleName() + "[", "]")
                .add("sofaSimpleTypeBean=" + sofaSimpleTypeBean)
                .add("idLists=" + idLists)
                .add("idMaps=" + idMaps)
                .toString();
    }

}
