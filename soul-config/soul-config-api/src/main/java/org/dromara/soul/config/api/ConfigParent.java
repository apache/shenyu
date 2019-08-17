/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 */

package org.dromara.soul.config.api;

/**
 * ConfigParent .
 * 所有自定义的配置信息的父类.
 * 2019-08-12 20:45
 *
 * @author chenbin sixh
 */
public abstract class ConfigParent {

    private volatile boolean isLoad = false;

    public void flagLoad() {
        isLoad = true;
    }

    public boolean isLoad() {
        return isLoad;
    }

    /**
     * yml file properties prefix.
     *
     * @return string. string
     */
    public abstract String prefix();
}
