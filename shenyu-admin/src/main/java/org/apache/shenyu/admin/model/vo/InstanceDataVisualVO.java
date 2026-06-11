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

package org.apache.shenyu.admin.model.vo;


import java.util.List;

public class InstanceDataVisualVO {

    private List<Entry> pieData;

    private List<InstanceDataVisualLineVO> lineData;

    /**
     * Gets the value of pieData.
     *
     * @return the value of pieData
     */
    public List<Entry> getPieData() {
        return pieData;
    }

    /**
     * set pieData.
     *
     * @param pieData pieData
     */
    public void setPieData(final List<Entry> pieData) {
        this.pieData = pieData;
    }

    /**
     * Gets the value of lineData.
     *
     * @return the value of lineData
     */
    public List<InstanceDataVisualLineVO> getLineData() {
        return lineData;
    }

    /**
     * set lineData.
     *
     * @param lineData lineData
     */
    public void setLineData(final List<InstanceDataVisualLineVO> lineData) {
        this.lineData = lineData;
    }

    public static class Entry {

        private String name;

        private Long value;

        public Entry(final String name, final Long value) {
            this.name = name;
            this.value = value;
        }

        /**
         * Gets the value of name.
         *
         * @return the value of name
         */
        public String getName() {
            return name;
        }

        /**
         * set name.
         *
         * @param name name
         */
        public void setName(final String name) {
            this.name = name;
        }

        /**
         * Gets the value .
         *
         * @return the value
         */
        public Long getValue() {
            return value;
        }

        /**
         * set value.
         *
         * @param value value
         */
        public void setValue(final Long value) {
            this.value = value;
        }
    }
}
