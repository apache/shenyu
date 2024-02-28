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

package org.apache.shenyu.common.enums;

/**
 * the api source.
 */
public enum ApiSourceEnum {

    /**
     * swagger.
     */
    SWAGGER("swagger", 0),

    /**
     * annotation_generation.
     */
    ANNOTATION_GENERATION("annotation_generation", 1),

    /**
     * create_manually.
     */
    CREATE_MANUALLY("create_manually", 2),

    /**
     * import_swagger.
     */
    IMPORT_SWAGGER("import_swagger", 3),

    /**
     * import_yapi.
     */
    IMPORT_YAPI("import_yapi", 4);

    private final String name;

    private final Integer value;

    /**
     * Construct.
     *
     * @param name  name
     * @param value value
     */
    ApiSourceEnum(final String name, final Integer value) {
        this.name = name;
        this.value = value;
    }

    /**
     * get name .
     *
     * @return name
     */
    public String getName() {
        return name;
    }

    /**
     * get value .
     *
     * @return value
     */
    public Integer getValue() {
        return value;
    }
}
