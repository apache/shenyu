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

package org.apache.shenyu.common.utils;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * Test cases for VersionUtils.
 */
public final class VersionUtilsTest {

    @Test
    public void testFromDefaultVersion() {
        String version = VersionUtils.getVersion();
        assertNotNull(version);
    }

    @Test
    public void testFromImplementationVersion() throws ClassNotFoundException {
        String version = VersionUtils.getVersion(Class.forName("java.lang.String"), "2.0.2");
        assertNotNull(version);
    }

    @Test
    public void testGetVersionWithDefaultFallback() {
        // Test with a class that has no manifest info - should return default version
        String defaultVersion = "1.2.3";
        String version = VersionUtils.getVersion(VersionUtilsTest.class, defaultVersion);
        assertNotNull(version);
    }

    @Test
    public void testGetVersionWithNullCodeSource() {
        // Test with a core Java class that might have null code source
        String defaultVersion = "default-version";
        String version = VersionUtils.getVersion(Object.class, defaultVersion);
        assertNotNull(version);
    }

    @Test
    public void testCheckDuplicateDoesNotThrow() {
        // checkDuplicate should not throw for a normal class
        VersionUtils.checkDuplicate(VersionUtils.class);
        // If we reach here, no exception was thrown
    }

    @Test
    public void testCheckDuplicateWithCoreClass() {
        // checkDuplicate should not throw for core Java classes
        VersionUtils.checkDuplicate(String.class);
        // If we reach here, no exception was thrown
    }

    @Test
    public void testGetVersionReturnsNonEmptyString() {
        String version = VersionUtils.getVersion();
        assertNotNull(version);
        // Version should not be empty
        assertFalse(version.isEmpty());
    }

    @Test
    public void testGetVersionWithCustomClassAndDefault() {
        // Test that default version is returned when no version info is available
        String customDefault = "custom-default-1.0.0";
        String version = VersionUtils.getVersion(VersionUtilsTest.class, customDefault);
        assertNotNull(version);
    }
}
