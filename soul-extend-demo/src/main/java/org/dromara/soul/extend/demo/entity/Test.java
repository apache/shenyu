/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 *
 */

package org.dromara.soul.extend.demo.entity;

import lombok.Data;
import org.dromara.soul.common.utils.GsonUtils;

/**
 * The type Test.
 *
 * @author xiaoyu(Myth)
 */
@Data
public class Test {

    private String id;

    private String name;

    /**
     * The entry point of application.
     *
     * @param args the input arguments
     */
    public static void main(String[] args) {
        Test test = new Test();
        test.setId("1");
        test.setName("xiaoyu");
        final String toJson = GsonUtils.getInstance().toJson(test);
        System.out.println(toJson);
    }
}
