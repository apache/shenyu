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

package org.apache.shenyu.admin.config.properties;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

/**
 * Local DataSource configuration.
 */
@Component
@ConfigurationProperties(prefix = "shenyu.database")
public class DataBaseProperties {

    private String dialect;

    private String initScript;

    private Boolean initEnable;

    /**
     * Gets the value of dialect.
     *
     * @return the value of dialect
     */
    public String getDialect() {
        return dialect;
    }

    /**
     * Sets the dialect.
     *
     * @param dialect dialect
     */
    public void setDialect(final String dialect) {
        this.dialect = dialect;
    }

    /**
     * Gets the value of initScript.
     *
     * @return the value of initScript
     */
    public String getInitScript() {
        return initScript;
    }

    /**
     * Sets the initScript.
     *
     * @param initScript initScript
     */
    public void setInitScript(final String initScript) {
        this.initScript = initScript;
    }

    /**
     * Gets the value of initEnable.
     *
     * @return the value of initEnable
     */
    public Boolean getInitEnable() {
        return initEnable;
    }

    /**
     * Sets the initEnable.
     *
     * @param initEnable initEnable
     */
    public void setInitEnable(final Boolean initEnable) {
        this.initEnable = initEnable;
    }
}
