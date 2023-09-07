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

package org.apache.shenyu.alert;

import org.apache.velocity.VelocityContext;
import org.apache.velocity.app.VelocityEngine;

import java.io.StringWriter;
import java.util.Map;

public class ContentConverter {

    /**
     * message format.
     * @param message message
     * @param params params
     * @return parameterized message.
     */
    public static String messageFormat(final String message, final Map<String, Object> params, final String name) {
        VelocityContext context = new VelocityContext();
        VelocityEngine velocityEngine = new VelocityEngine();
        StringWriter writer = new StringWriter();

        for (Map.Entry<String, Object> entry : params.entrySet()) {
            context.put(entry.getKey(), entry.getValue());
        }
        velocityEngine.evaluate(context, writer, name, message);

        return writer.toString();
    }

}
