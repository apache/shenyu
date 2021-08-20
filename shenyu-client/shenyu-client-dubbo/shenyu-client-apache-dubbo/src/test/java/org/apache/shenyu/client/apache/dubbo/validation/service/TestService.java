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

package org.apache.shenyu.client.apache.dubbo.validation.service;

import javax.validation.constraints.NotNull;
import java.util.Objects;

/**
 * TestService.
 */
public interface TestService {

    /**
     * test method.
     *
     * @param testObject testObject
     * @return String
     */
    String test(TestObject testObject);

    class TestObject {

        @NotNull(message = "age cannot be null.")
        private Integer age;

        /**
         * constructor without parameter.
         */
        public TestObject() {
        }

        /**
         * constructor with all params.
         *
         * @param age age
         */
        public TestObject(final Integer age) {
            this.age = age;
        }

        /**
         * get age.
         *
         * @return age
         */
        public Integer getAge() {
            return age;
        }

        /**
         * set age.
         *
         * @param age age
         */
        public void setAge(final Integer age) {
            this.age = age;
        }

        @Override
        public boolean equals(final Object o) {
            if (this == o) {
                return true;
            }
            if (o == null || getClass() != o.getClass()) {
                return false;
            }
            TestObject that = (TestObject) o;
            return getAge().equals(that.getAge());
        }

        @Override
        public int hashCode() {
            return Objects.hash(getAge());
        }

        @Override
        public String toString() {
            return "TestObject{"
                    + "age=" + age
                    + '}';
        }
    }
}
