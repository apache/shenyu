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

package org.apache.shenyu.agent.tracing.jaeger.advice;

import net.bytebuddy.asm.Advice;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;

/**
 * DoExecuteAdvice.
 */
public class DoExecuteAdvice {

    /**
     * onEnter.
     *
     * @param args args.
     * @param target the target class.
     */
    @Advice.OnMethodEnter(suppress = Throwable.class)
    public static void onEnter(
            @Advice.AllArguments final Object[] args,
            @Advice.This final AbstractShenyuPlugin target) {
        String pluginName = target.named();
    }

    /**
     * onExit.
     *
     * @param throwable throw.
     * @param args args.
     * @param target the target class.
     */
    @Advice.OnMethodExit(onThrowable = Throwable.class, suppress = Throwable.class)
    public static void onExit(
            @Advice.Thrown final Throwable throwable,
            @Advice.AllArguments final Object[] args,
            @Advice.This final Object target) {
    }
}
