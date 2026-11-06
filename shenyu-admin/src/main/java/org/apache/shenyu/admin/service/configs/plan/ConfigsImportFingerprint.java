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

import org.apache.commons.codec.digest.DigestUtils;
import org.apache.shenyu.admin.model.entity.NamespacePluginRelDO;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.entity.RuleDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.model.entity.ShenyuDictDO;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

/**
 * Stable digests for import preview archives and target identity snapshots.
 */
public final class ConfigsImportFingerprint {

    /**
     * Bump when fingerprint fields or preview token semantics change.
     */
    private static final String VERSION = "configs-import-preview-v1";

    private ConfigsImportFingerprint() {
    }

    /**
     * SHA-256 digest of the uploaded archive bytes.
     *
     * @param source archive bytes
     * @return digest with an algorithm prefix
     */
    public static String archiveDigest(final byte[] source) {
        return "sha256:" + DigestUtils.sha256Hex(source);
    }

    /**
     * Fingerprint the identity fields that determine create/reuse/skip decisions.
     *
     * @param namespace target namespace
     * @param snapshot target identity snapshot
     * @return digest with an algorithm prefix
     */
    public static String baseline(final String namespace, final ConfigsImportTargetSnapshot snapshot) {
        List<String> rows = new ArrayList<>(snapshot.plugins().size() + snapshot.namespacePlugins().size()
                + snapshot.selectors().size() + snapshot.rules().size() + snapshot.dicts().size());
        for (PluginDO plugin : snapshot.plugins()) {
            rows.add("plugin|" + field(plugin.getId()) + field(plugin.getName()));
        }
        for (NamespacePluginRelDO relation : snapshot.namespacePlugins()) {
            rows.add("namespace-plugin|" + field(relation.getId()) + field(relation.getPluginId()));
        }
        for (SelectorDO selector : snapshot.selectors()) {
            rows.add("selector|" + field(selector.getId()) + field(selector.getPluginId()) + field(selector.getSelectorName()));
        }
        for (RuleDO rule : snapshot.rules()) {
            rows.add("rule|" + field(rule.getId()) + field(rule.getSelectorId()) + field(rule.getRuleName()));
        }
        for (ShenyuDictDO dict : snapshot.dicts()) {
            rows.add("dict|" + field(dict.getId()) + field(dict.getType()) + field(dict.getDictValue())
                    + field(String.valueOf(dict.getEnabled())));
        }
        rows.sort(String::compareTo);
        StringBuilder canonical = new StringBuilder(VERSION);
        canonical.append(field(namespace));
        rows.forEach(canonical::append);
        return "sha256:" + DigestUtils.sha256Hex(canonical.toString().getBytes(StandardCharsets.UTF_8));
    }

    /**
     * Derive the optional token that binds a preview to its archive and baseline.
     *
     * @param namespace target namespace
     * @param archiveDigest archive digest
     * @param baselineFingerprint baseline fingerprint
     * @return digest with an algorithm prefix
     */
    public static String token(final String namespace, final String archiveDigest, final String baselineFingerprint) {
        String canonical = VERSION + field(namespace) + field(archiveDigest) + field(baselineFingerprint);
        return "sha256:" + DigestUtils.sha256Hex(canonical.getBytes(StandardCharsets.UTF_8));
    }

    private static String field(final String value) {
        if (Objects.isNull(value)) {
            return "-1:";
        }
        return value.length() + ":" + value;
    }
}
