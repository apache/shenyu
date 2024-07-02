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

package org.apache.shenyu.client.apache.dubbo.validation.mock;

import org.apache.dubbo.validation.MethodValidated;

import jakarta.validation.constraints.NotNull;
import java.util.List;
import java.util.Map;

/**
 * MockValidatorTarget.
 */
public interface MockValidatorTarget {

    /**
     * mock method: methodOne.
     *
     * @param anything parameter
     */
    @MethodValidated
    void methodOne(String anything);

    /**
     * mock method: methodTwo.
     *
     * @param mockValidationParameter parameter
     */
    @MethodValidated
    void methodTwo(@NotNull @MockConstraint MockValidationParameter mockValidationParameter);

    /**
     * mock method: methodThree.
     *
     * @param parameters parameter
     */
    void methodThree(MockValidationParameter[] parameters);

    /**
     * mock method: methodFour.
     *
     * @param strings parameter
     */
    void methodFour(List<String> strings);

    /**
     * mock method: methodFive.
     *
     * @param map parameter
     */
    void methodFive(Map<String, String> map);

    interface MethodOne {

    }

    interface MethodTwo {

    }

    interface MethodThree {

    }

    interface MethodFour {

    }

    interface MethodFive {

    }
}
