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

package org.apache.shenyu.protocol.mqtt;

import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.protocol.mqtt.repositories.BaseRepository;
import org.reflections.Reflections;

/**
 * mqtt server.
 */
public class MqttBootstrapServer implements BootstrapServer {

    private static final String REPOSITORY_PACKAGE_NAME = "org.apache.shenyu.protocol.mqtt.repositories";

    @Override
    public void init() {
        try {
            initRepositories();
        } catch (Exception e) {
            //// todo log
        }
    }

    @Override
    public void start() {

    }

    @Override
    public void shutdown() {

    }

    private void initRepositories() throws IllegalAccessException, InstantiationException {
        Reflections reflections = new Reflections(REPOSITORY_PACKAGE_NAME);
        for (Class<? extends BaseRepository> clazz : reflections.getSubTypesOf(BaseRepository.class)) {
            Singleton.INST.single(clazz, clazz.newInstance());
        }
    }

}
