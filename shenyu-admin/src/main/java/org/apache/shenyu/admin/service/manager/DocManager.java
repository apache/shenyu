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
import org.apache.shenyu.admin.model.bean.DocItem;

/**
 * Doc Manager.
 */
public interface DocManager {

    /**
     * addDocInfo.
     *
     * @param serviceId serviceId
     * @param docJson docJson
     * @param callback callback
     */
    void addDocInfo(String serviceId, String docJson, Consumer<DocInfo> callback);

    /**
     * get docInfo by title.
     *
     * @param title title
     * @return DocInfo
     */
    DocInfo getByTitle(String title);

    /**
     * getDocItem.
     *
     * @param id id
     * @return DocItem
     */
    DocItem getDocItem(String id);

    /**
     * listAll.
     *
     * @return Collection
     */
    Collection<DocInfo> listAll();

    /**
     * getDocMd5.
     *
     * @param serviceId serviceId
     * @return String
     */
    String getDocMd5(String serviceId);

    /**
     * remove.
     *
     * @param serviceId serviceId
     */
    void remove(String serviceId);
}
