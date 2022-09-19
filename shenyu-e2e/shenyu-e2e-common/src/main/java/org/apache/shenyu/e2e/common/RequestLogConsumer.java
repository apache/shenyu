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

package org.apache.shenyu.e2e.common;

import io.restassured.filter.log.LogDetail;
import io.restassured.filter.log.UrlDecoder;
import io.restassured.internal.print.RequestPrinter;
import io.restassured.specification.FilterableRequestSpecification;
import org.slf4j.Logger;

import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;
import java.nio.charset.Charset;
import java.util.Collections;

public class RequestLogConsumer {
    private static final OutputStream FAKE_STREAM = new OutputStream() {
        @Override
        public void write(int b) throws IOException {
        
        }
    };
    
    public static void print(Logger logger, FilterableRequestSpecification requestSpec) {
        String uri = UrlDecoder.urlDecode(
                requestSpec.getURI(),
                Charset.forName(requestSpec.getConfig().getEncoderConfig().defaultQueryParameterCharset()),
                true
        );
        
        String logContent = RequestPrinter.print(
                requestSpec,
                requestSpec.getMethod(),
                uri,
                LogDetail.ALL,
                Collections.emptySet(),
                new PrintStream(FAKE_STREAM),
                true
        );
        logger.debug(logContent);
    }
}
