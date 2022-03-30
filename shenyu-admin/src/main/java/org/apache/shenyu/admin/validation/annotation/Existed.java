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

package org.apache.shenyu.admin.validation.annotation;

import org.apache.shenyu.admin.validation.ExistProvider;
import org.apache.shenyu.admin.validation.validator.ExistedValidator;

import javax.validation.Constraint;
import javax.validation.Payload;
import java.lang.annotation.Documented;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.Target;

import static java.lang.annotation.ElementType.ANNOTATION_TYPE;
import static java.lang.annotation.ElementType.CONSTRUCTOR;
import static java.lang.annotation.ElementType.FIELD;
import static java.lang.annotation.ElementType.METHOD;
import static java.lang.annotation.ElementType.PARAMETER;
import static java.lang.annotation.ElementType.TYPE_USE;
import static java.lang.annotation.RetentionPolicy.RUNTIME;

/**
 * Existed.
 */
@Target({METHOD, FIELD, ANNOTATION_TYPE, CONSTRUCTOR, PARAMETER, TYPE_USE})
@Retention(RUNTIME)
@Repeatable(Existed.List.class)
@Documented
@Constraint(validatedBy = ExistedValidator.class)
public @interface Existed {

    /**
     * default provider method name.
     */
    String EXISTED = "existed";
    /**
     * if null,valid is ignore.
     *
     * @return not existed
     */
    boolean nullOfIgnore() default false;
    
    /**
     * if reverse ,it is not existed.
     *
     * @return not existed
     */
    boolean reverse() default false;
    
    /**
     * message.
     *
     * @return string
     */
    String message() default "the key is not existed!";
    
    /**
     * existed provider.
     *
     * @return class
     */
    Class<? extends ExistProvider> provider();

    /**
     * existed provider.
     *
     * @return class
     */
    String providerMethodName() default EXISTED;

    /**
     * support groups.
     *
     * @return class array.
     */
    Class<?>[] groups() default {};
    
    /**
     * payload.
     *
     * @return Payload class array
     */
    Class<? extends Payload>[] payload() default {};
    
    
    @Target({METHOD, FIELD, ANNOTATION_TYPE, CONSTRUCTOR, PARAMETER, TYPE_USE})
    @Retention(RUNTIME)
    @Documented
    @interface List {
        
        Existed[] value();
    }
}
