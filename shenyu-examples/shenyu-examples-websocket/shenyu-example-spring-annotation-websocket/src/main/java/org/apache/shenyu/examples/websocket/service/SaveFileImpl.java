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

package org.apache.shenyu.examples.websocket.service;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import java.io.File;
import java.io.FileOutputStream;
import java.util.Map;

@Service
public class SaveFileImpl implements SaveFile {

    private static final Logger LOG = LoggerFactory.getLogger(SaveFileImpl.class);

    @Override
    public boolean saveFileFromBytes(final byte[] b, final Map<String, Object> map) {
        //从map中获取file对象
        File file = (File) map.get("file");
        //判断路径是否存在，不存在就创建
        if (!file.getParentFile().exists()) {
            file.getParentFile().mkdirs();
        }

        try (FileOutputStream fstream = new FileOutputStream(file, true)) {
            fstream.write(b);
        } catch (Exception e) {
            LOG.error("saveFileFromBytes error", e);
            return false;
        }
        return true;
    }
}
