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


/**
 * CustomCode.
 */
public class CustomCode {

    private String code;

    private String message;

    private String solution;


    /**
     * getCode.
     *
     * @return String
     */
    public String getCode() {
        return code;
    }

    /**
     * setCode.
     *
     * @param code code
     */
    public void setCode(final String code) {
        this.code = code;
    }

    /**
     * getMessage.
     *
     * @return String
     */
    public String getMessage() {
        return message;
    }

    /**
     * setMessage.
     *
     * @param message message
     */
    public void setMessage(final String message) {
        this.message = message;
    }

    /**
     * getSolution.
     *
     * @return String
     */
    public String getSolution() {
        return solution;
    }

    /**
     * setSolution.
     *
     * @param solution solution
     */
    public void setSolution(final String solution) {
        this.solution = solution;
    }
}
