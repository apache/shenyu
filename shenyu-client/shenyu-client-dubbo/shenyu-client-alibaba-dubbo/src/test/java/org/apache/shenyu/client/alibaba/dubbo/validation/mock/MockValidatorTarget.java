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

package org.apache.shenyu.client.alibaba.dubbo.validation.mock;

import com.alibaba.dubbo.validation.MethodValidated;

import javax.validation.constraints.NotNull;
import java.util.List;
import java.util.Map;

/**
 * Mock Validator Target.
 */
public interface MockValidatorTarget {
    
    /**
     * mock method: method1.
     *
     * @param anything parameter
     */
    @MethodValidated
    void method1(String anything);

    /**
     * mock method: method2.
     *
     * @param mockValidationParameter parameter
     */
    @MethodValidated
    void method2(@NotNull @MockConstraint MockValidationParameter mockValidationParameter);

    /**
     * mock method: method3.
     *
     * @param parameters parameter
     */
    void method3(MockValidationParameter[] parameters);

    /**
     * mock method: method4.
     *
     * @param strings parameter
     */
    void method4(List<String> strings);

    /**
     * mock method: method5.
     *
     * @param map parameter
     */
    void method5(Map<String, String> map);

    interface Method1 {

    }

    interface Method2 {

    }

    interface Method3 {

    }

    interface Method4 {

    }

    interface Method5 {

    }
}
