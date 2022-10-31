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

import javax.validation.Constraint;
import javax.validation.Payload;
import java.lang.annotation.Documented;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import static java.lang.annotation.ElementType.ANNOTATION_TYPE;
import static java.lang.annotation.ElementType.CONSTRUCTOR;
import static java.lang.annotation.ElementType.FIELD;
import static java.lang.annotation.ElementType.METHOD;
import static java.lang.annotation.ElementType.PARAMETER;
import static java.lang.annotation.ElementType.TYPE_USE;

/**
 * MockConstraint.
 */
@Target({METHOD, FIELD, ANNOTATION_TYPE, CONSTRUCTOR, PARAMETER, TYPE_USE})
@Retention(RetentionPolicy.RUNTIME)
@Documented
@Constraint(validatedBy = MockConstraintValidator.class)
public @interface MockConstraint {

    /**
     * mock message parameter.
     *
     * @return the mock value
     */
    String message() default "{message}";

    /**
     * mock class array parameter.
     *
     * @return the mock value
     */
    Class<?>[] groups() default {};

    /**
     * constraint required payload.
     *
     * @return the payload value
     */
    Class<? extends Payload>[] payload() default {};

    /**
     * mock byte parameter.
     *
     * @return the mock value
     */
    byte mpByte() default '1';

    /**
     * mock short parameter.
     *
     * @return the mock value
     */
    short mpShort() default 1;

    /**
     * mock char parameter.
     *
     * @return the mock value
     */
    char mpChar() default ' ';

    /**
     * mock int parameter.
     *
     * @return the mock value
     */
    int mpInteger() default 0;

    /**
     * mock class parameter.
     *
     * @return the mock value
     */
    Class<?> mpClass() default Object.class;

    /**
     * mock class parameter.
     *
     * @return the mock value
     */
    MockEnumType mpEnum() default MockEnumType.MEMBER;

    /**
     * mock boolean parameter.
     *
     * @return the mock value
     */
    boolean mpBool() default false;

    /**
     * mock long parameter.
     *
     * @return the mock value
     */
    long mpLong() default 0;

    /**
     * mock float parameter.
     *
     * @return the mock value
     */
    float mpFloat() default 0;

    /**
     * mock double parameter.
     *
     * @return the mock value
     */
    double mpDouble() default 0;

    /**
     * mock object array parameter.
     *
     * @return the mock value
     */
    Class<?>[] mpClassArray() default {Object.class};
}
