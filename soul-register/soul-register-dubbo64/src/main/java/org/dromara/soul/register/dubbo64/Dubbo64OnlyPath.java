/*
 *     Licensed to the Apache Software Foundation (ASF) under one or more
 *     contributor license agreements.See the NOTICE file distributed with
 *     this work for additional information regarding copyright ownership.
 *     The ASF licenses this file to You under the Apache License, Version 2.0
 *     (the "License"); you may not use this file except in compliance with
 *     the License.You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *     Unless required by applicable law or agreed to in writing, software
 *     distributed under the License is distributed on an "AS IS" BASIS,
 *     WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *     See the License for the specific language governing permissions and
 *     limitations under the License.
 */

package org.dromara.soul.register.dubbo64;

import lombok.Data;
import org.dromara.soul.register.api.Path;

/**
 * Dubbo64OnlyPath .
 * Dubbo 64 version related, path processing.
 *
 * @author sixh
 */
@Data
public class Dubbo64OnlyPath implements Path {

    private String application;

    private String group;

    private String service;

    private String method;

    private String version;
}
