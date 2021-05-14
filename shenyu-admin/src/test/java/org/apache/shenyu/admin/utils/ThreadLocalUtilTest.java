/*
 * Copyright 2007-present the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.shenyu.admin.utils;

import org.junit.Test;

import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertThat;

public class ThreadLocalUtilTest {

    @Test
    public void testPutValue() {
        ThreadLocalUtil.put("threadKey", "threadValue");
        assertThat("threadValue", is(ThreadLocalUtil.get("threadKey")));
    }

    @Test
    public void testGetValue() {
        ThreadLocalUtil.put("threadKey", "threadValue");
        assertThat("threadValue", is(ThreadLocalUtil.get("threadKey")));
    }

    @Test
    public void testRemove() {
        ThreadLocalUtil.put("threadKey", "threadValue");
        assertThat("threadValue", is(ThreadLocalUtil.get("threadKey")));
        ThreadLocalUtil.remove("threadKey");
        assertThat(ThreadLocalUtil.get("threadKey"), nullValue());
    }

    @Test
    public void testClear() {
        ThreadLocalUtil.put("threadValue", "threadValue");
        ThreadLocalUtil.clear();
        assertThat(ThreadLocalUtil.get("threadLocalKey"), nullValue());
    }
}
