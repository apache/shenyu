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

package org.apache.shenyu.common.config;

/**
 * Finite gateway extension loading budgets, in bytes except for the entry count.
 * Configure under {@code shenyu.ext-plugin.archive-limits} before deploying larger
 * existing extensions. Limits also apply to Admin-synchronized artifacts even when
 * filesystem scanning is disabled. They bound archive input and expanded payload,
 * not total heap usage or the earlier data-sync transport/deserializer allocation.
 * Parsing maps, metadata and temporary byte-array copies consume additional memory.
 */
public final class PluginArchiveLimits {

    /**
     * Compressed archive input; default 16 MiB.
     */
    private int maxArchiveBytes = 16 * 1024 * 1024;

    /**
     * All entries, including directories and duplicate names; default 10000.
     */
    private int maxEntries = 10000;

    /**
     * Expanded bytes of each entry, including metadata; default 16 MiB.
     */
    private int maxEntryBytes = 16 * 1024 * 1024;

    /**
     * Sum of all expanded entry bytes; default 64 MiB.
     */
    private int maxExpandedBytes = 64 * 1024 * 1024;

    /**
     * Get maxArchiveBytes.
     * @return positive limit
     */
    public int getMaxArchiveBytes() {
        return maxArchiveBytes;
    }

    /**
     * Set maxArchiveBytes; zero and negative values never disable accounting.
     * @param maxArchiveBytes positive limit
     */
    public void setMaxArchiveBytes(final int maxArchiveBytes) {
        if (maxArchiveBytes <= 0 || maxArchiveBytes > Integer.MAX_VALUE - 8) {
            throw new IllegalArgumentException("maxArchiveBytes must be between 1 and Integer.MAX_VALUE - 8");
        }
        this.maxArchiveBytes = maxArchiveBytes;
    }

    /**
     * Get maxEntries.
     * @return positive limit
     */
    public int getMaxEntries() {
        return maxEntries;
    }

    /**
     * Set maxEntries; zero and negative values never disable accounting.
     * @param maxEntries positive limit
     */
    public void setMaxEntries(final int maxEntries) {
        if (maxEntries <= 0 || maxEntries > Integer.MAX_VALUE - 8) {
            throw new IllegalArgumentException("maxEntries must be between 1 and Integer.MAX_VALUE - 8");
        }
        this.maxEntries = maxEntries;
    }

    /**
     * Get maxEntryBytes.
     * @return positive limit
     */
    public int getMaxEntryBytes() {
        return maxEntryBytes;
    }

    /**
     * Set maxEntryBytes; zero and negative values never disable accounting.
     * @param maxEntryBytes positive limit
     */
    public void setMaxEntryBytes(final int maxEntryBytes) {
        if (maxEntryBytes <= 0 || maxEntryBytes > Integer.MAX_VALUE - 8) {
            throw new IllegalArgumentException("maxEntryBytes must be between 1 and Integer.MAX_VALUE - 8");
        }
        this.maxEntryBytes = maxEntryBytes;
    }

    /**
     * Get maxExpandedBytes.
     * @return positive limit
     */
    public int getMaxExpandedBytes() {
        return maxExpandedBytes;
    }

    /**
     * Set maxExpandedBytes; zero and negative values never disable accounting.
     * @param maxExpandedBytes positive limit
     */
    public void setMaxExpandedBytes(final int maxExpandedBytes) {
        if (maxExpandedBytes <= 0 || maxExpandedBytes > Integer.MAX_VALUE - 8) {
            throw new IllegalArgumentException("maxExpandedBytes must be between 1 and Integer.MAX_VALUE - 8");
        }
        this.maxExpandedBytes = maxExpandedBytes;
    }

}
