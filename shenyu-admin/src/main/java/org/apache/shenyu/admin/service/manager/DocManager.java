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

package org.apache.shenyu.admin.service.manager;

import java.util.Collection;
import java.util.function.Consumer;
import org.apache.shenyu.admin.model.bean.DocInfo;
import org.apache.shenyu.admin.model.bean.UpstreamInstance;

/**
 * Doc Manager.
 */
public interface DocManager {

    /**
     * addDocInfo.
     *
     * @param instance instance
     * @param docJson  docJson
     * @param oldMd5   oldMd5
     * @param callback callback
     */
    void addDocInfo(UpstreamInstance instance, String docJson, String oldMd5, Consumer<DocInfo> callback);

    /**
     * listAll.
     *
     * @return Collection
     */
    Collection<DocInfo> listAll();
}
