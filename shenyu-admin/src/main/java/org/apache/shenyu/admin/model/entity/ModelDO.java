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

package org.apache.shenyu.admin.model.entity;

import java.sql.Timestamp;
import java.util.Objects;

/**
 * model.
 */
public class ModelDO extends BaseDO {

    /**
     * the model name.
     */
    private String name;

    /**
     * the model description.
     */
    private String modelDesc;

    /**
     * getName.
     *
     * @return name
     */
    public String getName() {
        return name;
    }

    /**
     * set name.
     *
     * @param name name.
     */
    public void setName(final String name) {
        this.name = name;
    }

    /**
     * getModelDesc.
     *
     * @return modelDesc
     */
    public String getModelDesc() {
        return modelDesc;
    }

    /**
     * set model desc.
     *
     * @param modelDesc modelDesc.
     */
    public void setModelDesc(final String modelDesc) {
        this.modelDesc = modelDesc;
    }

    /**
     * builder.
     *
     * @return ModelDO.ModelDOBuilder
     */
    public static ModelDO.ModelDOBuilder builder() {
        return new ModelDO.ModelDOBuilder();
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        if (!super.equals(o)) {
            return false;
        }
        ModelDO modelDO = (ModelDO) o;
        return Objects.equals(name, modelDO.name)
                && Objects.equals(modelDesc, modelDO.modelDesc);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), name, modelDesc);
    }

    public static final class ModelDOBuilder {

        private String id;

        private String name;

        private String modelDesc;

        private Timestamp dateCreated;

        private Timestamp dateUpdated;

        private ModelDOBuilder() {
        }

        /**
         * id.
         *
         * @param id the id.
         * @return ModelDOBuilder.
         */
        public ModelDO.ModelDOBuilder id(final String id) {
            this.id = id;
            return this;
        }

        /**
         * name.
         *
         * @param name the name
         * @return ModelDOBuilder
         */
        public ModelDO.ModelDOBuilder name(final String name) {
            this.name = name;
            return this;
        }

        /**
         * modelDesc.
         *
         * @param modelDesc the modelDesc
         * @return ModelDOBuilder
         */
        public ModelDO.ModelDOBuilder modelDesc(final String modelDesc) {
            this.modelDesc = modelDesc;
            return this;
        }

        /**
         * dateCreated.
         *
         * @param dateCreated the dateCreated
         * @return ModelDOBuilder
         */
        public ModelDO.ModelDOBuilder dateCreated(final Timestamp dateCreated) {
            this.dateCreated = dateCreated;
            return this;
        }

        /**
         * dateUpdated.
         *
         * @param dateUpdated the dateUpdated
         * @return ModelDOBuilder
         */
        public ModelDO.ModelDOBuilder dateUpdated(final Timestamp dateUpdated) {
            this.dateUpdated = dateUpdated;
            return this;
        }

        /**
         * build.
         *
         * @return ModelDO
         */
        public ModelDO build() {
            ModelDO modelDO = new ModelDO();
            modelDO.setId(this.id);
            modelDO.setName(this.name);
            modelDO.setModelDesc(this.modelDesc);
            modelDO.setDateCreated(this.dateCreated);
            modelDO.setDateUpdated(this.dateUpdated);
            return modelDO;
        }

    }

}
