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

package org.apache.shenyu.admin.service.configs.plan;

import org.apache.shenyu.admin.model.result.ConfigsImportPreview;

/**
 * Read-only planning for namespace configuration import archives.
 */
public interface ConfigsImportPreviewService {

    /**
     * Build a preview without writing configuration or publishing events.
     *
     * @param namespace target namespace
     * @param source archive bytes
     * @return preview report
     */
    ConfigsImportPreview preview(String namespace, byte[] source);

    /**
     * Check that a preview token still matches the archive and target baseline.
     *
     * @param namespace target namespace
     * @param source archive bytes
     * @param previewToken token returned by {@link #preview(String, byte[])}
     * @return true when the token is current
     */
    boolean matchesToken(String namespace, byte[] source, String previewToken);
}
