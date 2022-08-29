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

package org.apache.shenyu.examples.motan.service;

import com.weibo.api.motan.transport.async.MotanAsync;

/**
 * Motan demo interface.
 */
@MotanAsync
public interface MotanDemoService {

    /**
     * hello demo for Motan.
     * @param name  the name to hello
     * @return  response
     */
    String hello(String name);

    /**
     * timeout for Motan.
     * @param seconds  seconds to timeout
     * @return  response
     */
    String testTimeOut(long seconds);
}
