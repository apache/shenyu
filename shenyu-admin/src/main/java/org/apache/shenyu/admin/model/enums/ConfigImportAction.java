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

package org.apache.shenyu.admin.model.enums;

/**
 * The outcome of one source object in a configuration import plan.
 */
public enum ConfigImportAction {

    /**
     * The import would insert a new object.
     */
    CREATE,

    /**
     * The import would not insert; the source maps to an existing or planned object.
     */
    REUSE,

    /**
     * The import would ignore the source object without creating or remapping it.
     */
    SKIP,

    /**
     * The source object cannot be imported because it is invalid or lacks a dependency.
     */
    REJECT
}
