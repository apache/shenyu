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

package org.dromara.soul.register.common.init;

import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.stereotype.Component;

/**
 * The executor after the soul-register-client's host service is started.
 *
 * @author jerbo
 */
@Component
public class ApplicationSoulRegisterClientRunner implements ApplicationRunner {

    private static boolean started = Boolean.FALSE;

    /**
     * get started.
     * @return started
     */
    public static boolean isStarted() {
        return started;
    }

    @Override
    public void run(final ApplicationArguments args) {
        started = true;
    }
}
