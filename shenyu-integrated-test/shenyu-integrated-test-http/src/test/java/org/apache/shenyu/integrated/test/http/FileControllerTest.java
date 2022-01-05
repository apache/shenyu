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

package org.apache.shenyu.integrated.test.http;

import org.apache.shenyu.integratedtest.common.helper.FileUploadUtil;
import org.apache.shenyu.integratedtest.common.helper.HttpHelper;
import org.junit.Test;
import static org.junit.Assert.assertEquals;
import java.io.File;
import java.io.IOException;

public class FileControllerTest {
    @Test
    public void testFileUploadWay1() throws IOException {
        File file1 = new File("src/main/resources/test.txt");
        File file2 = new File("src/main/resources/test");
        String filePath = "shenyu-examples/shenyu-examples-http/src/main/resources/static/";
        assertEquals("TestSuccessful", FileUploadUtil.post(file1, filePath));
        assertEquals("TestFailed", FileUploadUtil.post(file2, filePath));
    }

    @Test
    public void testFileUploadWay2() throws IOException {
        String res1 = HttpHelper.INSTANCE.postGateway("/http/file/uploadWay2?file=TestSuccessful&fileName=test.txt&filePath=shenyu-examples/shenyu-examples-http/src/main/resources/static/",
                java.lang.String.class);
        assertEquals("UploadSucceeded", res1);
        String res2 = HttpHelper.INSTANCE.postGateway("/http/file/uploadWay2?file=TestSuccessful&fileName=test.txt&filePath=shenyu/image/ss/", java.lang.String.class);
        assertEquals("UploadFailed", res2);
    }

    @Test
    public void testFileDownload() throws IOException {
        String res = HttpHelper.INSTANCE.postGateway("/http/file/download", java.lang.String.class);
        assertEquals("TestSuccessful", res);
    }
}
