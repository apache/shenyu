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

package org.dromara.soul.client.alibaba.dubbo.validation;

import com.alibaba.dubbo.common.URL;
import org.dromara.soul.client.alibaba.dubbo.validation.service.TestService;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

/**
 * AlibabaDubboClientValidatorTest.
 *
 * @author KevinClair
 */
public class AlibabaDubboClientValidatorTest {

    /**
     * test method {@link AlibabaDubboClientValidator#validate(java.lang.String, java.lang.Class[], java.lang.Object[])}.
     */
    @Test
    public void validate() {
        URL url = URL.valueOf("dubbo://127.0.0.1:20880/org.dromara.soul"
                + ".client.alibaba.dubbo.validation.service.TestService"
                + "?accepts=500&anyhost=true&application=soul-proxy&bind.ip=127.0.0.1"
                + "&bind.port=20880&deprecated=false&dubbo=2.0.2&dynamic=true&generic=false"
                + "&interface=org.dromara.soul.client.alibaba.dubbo.validation.service.TestService"
                + "&keep.alive=true&methods=test&pid=67352&qos.enable=false&release=2.7.0"
                + "&side=provider&threadpool=fixed&threads=500&timeout=20000"
                + "&timestamp=1608119259859&validation=soulValidation");
        AlibabaDubboClientValidator alibabaDubboClientValidator = new AlibabaDubboClientValidator(url);
        try {
            alibabaDubboClientValidator.validate("test",
                    new Class[]{TestService.TestObject.class},
                    new Object[]{TestService.TestObject.builder().age(null).build()});
        } catch (Exception e) {
            assertEquals("age cannot be null.", e.getMessage());
        }
    }
}
