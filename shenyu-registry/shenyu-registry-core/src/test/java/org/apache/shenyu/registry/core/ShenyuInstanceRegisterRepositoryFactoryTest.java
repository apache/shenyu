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

package org.apache.shenyu.registry.core;

import org.apache.shenyu.registry.api.ShenyuInstanceRegisterRepository;
import org.apache.shenyu.registry.api.config.RegisterConfig;
import org.apache.shenyu.registry.api.entity.InstanceEntity;
import org.apache.shenyu.spi.ExtensionLoader;
import org.apache.shenyu.spi.Join;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.lang.reflect.Field;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Enumeration;
import java.util.Map;
import java.util.Objects;
import java.util.Properties;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class ShenyuInstanceRegisterRepositoryFactoryTest {

    private static final String SPI_RESOURCE = "META-INF/shenyu/" + ShenyuInstanceRegisterRepository.class.getName();

    @TempDir
    private Path directory;

    private URLClassLoader classLoader;

    @BeforeEach
    void setUp() throws Exception {
        clearCaches();
    }

    @AfterEach
    void tearDown() throws Exception {
        clearCaches();
        if (Objects.nonNull(classLoader)) {
            classLoader.close();
        }
    }

    @Test
    void noProviderGivesDependencyHint() throws Exception {
        useProviders("");
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class,
                () -> ShenyuInstanceRegisterRepositoryFactory.newInstance("zookeeper", classLoader));
        assertTrue(exception.getMessage().contains("shenyu-registry-zookeeper"));
        assertTrue(exception.getMessage().contains("runtime classpath"));
        assertTrue(exception.getMessage().contains("Available types: []"));
    }

    @Test
    void oneProviderIsLoadedAndCached() throws Exception {
        useProviders(provider("external"));
        ShenyuInstanceRegisterRepository repository = ShenyuInstanceRegisterRepositoryFactory.newInstance("external", classLoader);
        assertEquals(TestRepository.class, repository.getClass());
        assertSame(repository, ShenyuInstanceRegisterRepositoryFactory.newInstance("external", classLoader));
        assertNotSame(repository, ShenyuInstanceRegisterRepositoryFactory.loadRepository("external", classLoader));
    }

    @Test
    void multipleExternalProvidersKeepTheirSpiKeys() throws Exception {
        useProviders(provider("consul") + provider("polaris"));
        ShenyuInstanceRegisterRepository consul = ShenyuInstanceRegisterRepositoryFactory.newInstance("consul", classLoader);
        ShenyuInstanceRegisterRepository polaris = ShenyuInstanceRegisterRepositoryFactory.newInstance("polaris", classLoader);
        assertNotSame(consul, polaris);
        assertSame(consul, ShenyuInstanceRegisterRepositoryFactory.newInstance("consul", classLoader));
        assertSame(polaris, ShenyuInstanceRegisterRepositoryFactory.newInstance("polaris", classLoader));
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class,
                () -> ShenyuInstanceRegisterRepositoryFactory.newInstance("unknown", classLoader));
        assertTrue(exception.getMessage().contains("Available types: [consul, polaris]"));
    }

    @Test
    void missingProviderFailsFromEveryFactoryEntryPoint() throws Exception {
        useProviders(provider("external"));
        RegisterConfig config = config("missing");
        assertThrows(IllegalArgumentException.class, () -> ShenyuInstanceRegisterRepositoryFactory.loadRepository("missing", classLoader));
        assertThrows(IllegalArgumentException.class, () -> ShenyuInstanceRegisterRepositoryFactory.newAndInitInstance(config, classLoader));
        assertThrows(IllegalArgumentException.class, () -> ShenyuInstanceRegisterRepositoryFactory.reNewAndInitInstance(config, classLoader));
    }

    @Test
    void blankTypeFailsBeforeCacheAccess() {
        for (String type : new String[] {null, "", " "}) {
            assertThrows(IllegalArgumentException.class, () -> ShenyuInstanceRegisterRepositoryFactory.newInstance(type));
            assertThrows(IllegalArgumentException.class, () -> ShenyuInstanceRegisterRepositoryFactory.loadRepository(type));
            assertThrows(IllegalArgumentException.class, () -> ShenyuInstanceRegisterRepositoryFactory.newAndInitInstance(config(type)));
            assertThrows(IllegalArgumentException.class, () -> ShenyuInstanceRegisterRepositoryFactory.reNewAndInitInstance(config(type)));
            assertThrows(IllegalArgumentException.class, () -> ShenyuInstanceRegisterRepositoryFactory.newInstance(type, classLoader));
            assertThrows(IllegalArgumentException.class, () -> ShenyuInstanceRegisterRepositoryFactory.loadRepository(type, classLoader));
            assertThrows(IllegalArgumentException.class, () -> ShenyuInstanceRegisterRepositoryFactory.newAndInitInstance(config(type), classLoader));
            assertThrows(IllegalArgumentException.class, () -> ShenyuInstanceRegisterRepositoryFactory.reNewAndInitInstance(config(type), classLoader));
        }
    }

    @Test
    void initializationAndRenewalPreserveCacheContract() throws Exception {
        useProviders(provider("external"));
        RegisterConfig config = config("external");
        TestRepository repository = (TestRepository) ShenyuInstanceRegisterRepositoryFactory.newAndInitInstance(config, classLoader);
        assertSame(config, repository.config);
        assertEquals(1, repository.initCount);
        assertSame(repository, ShenyuInstanceRegisterRepositoryFactory.newAndInitInstance(config, classLoader));
        assertEquals(1, repository.initCount);
        RegisterConfig replacement = config("external");
        replacement.setServerLists("replacement");
        TestRepository renewed = (TestRepository) ShenyuInstanceRegisterRepositoryFactory.reNewAndInitInstance(replacement, classLoader);
        assertNotSame(repository, renewed);
        assertSame(replacement, renewed.config);
        assertSame(renewed, ShenyuInstanceRegisterRepositoryFactory.newInstance("external", classLoader));
    }

    @Test
    void uninitializedCachedInstanceKeepsExistingBehavior() throws Exception {
        useProviders(provider("external"));
        TestRepository repository = (TestRepository) ShenyuInstanceRegisterRepositoryFactory.newInstance("external", classLoader);
        assertSame(repository, ShenyuInstanceRegisterRepositoryFactory.newAndInitInstance(config("external"), classLoader));
        assertEquals(0, repository.initCount);
    }

    @Test
    void failedInitializationIsNotCachedAndFailedRenewalKeepsPreviousInstance() throws Exception {
        useProviders(provider("external"));
        RegisterConfig failing = config("external");
        failing.setServerLists("fail");
        assertThrows(IllegalStateException.class, () -> ShenyuInstanceRegisterRepositoryFactory.newAndInitInstance(failing, classLoader));
        TestRepository repository = (TestRepository) ShenyuInstanceRegisterRepositoryFactory.newAndInitInstance(config("external"), classLoader);
        assertEquals(1, repository.initCount);
        assertThrows(IllegalStateException.class, () -> ShenyuInstanceRegisterRepositoryFactory.reNewAndInitInstance(failing, classLoader));
        assertSame(repository, ShenyuInstanceRegisterRepositoryFactory.newInstance("external", classLoader));
    }

    @Test
    void externalProvidersDoNotLeakIntoDefaultLoader() throws Exception {
        useProviders(provider("external"));
        assertEquals(TestRepository.class, ShenyuInstanceRegisterRepositoryFactory.loadRepository("external", classLoader).getClass());
        assertThrows(IllegalArgumentException.class, () -> ShenyuInstanceRegisterRepositoryFactory.loadRepository("external"));
    }

    private void useProviders(final String definitions) throws IOException {
        Path resource = directory.resolve(SPI_RESOURCE);
        Files.createDirectories(resource.getParent());
        Files.write(resource, definitions.getBytes(StandardCharsets.UTF_8));
        classLoader = new URLClassLoader(new URL[] {directory.toUri().toURL()}, getClass().getClassLoader()) {
            @Override
            public Enumeration<URL> getResources(final String name) throws IOException {
                if (SPI_RESOURCE.equals(name)) {
                    return findResources(name);
                }
                return super.getResources(name);
            }
        };
    }

    private String provider(final String type) {
        return type + "=" + TestRepository.class.getName() + "\n";
    }

    private RegisterConfig config(final String type) {
        return new RegisterConfig(type, "server", new Properties());
    }

    private void clearCaches() throws Exception {
        Field repositories = ShenyuInstanceRegisterRepositoryFactory.class.getDeclaredField("REPOSITORY_MAP");
        repositories.setAccessible(true);
        ((Map<?, ?>) repositories.get(null)).clear();
        if (Objects.nonNull(classLoader)) {
            ExtensionLoader.removeExtensionLoader(ShenyuInstanceRegisterRepository.class, classLoader);
        }
    }

    /**
     * A provider loaded from an external SPI resource, without registry clients.
     */
    @Join(isSingleton = false)
    public static class TestRepository implements ShenyuInstanceRegisterRepository {

        private RegisterConfig config;

        private int initCount;

        @Override
        public void init(final RegisterConfig registerConfig) {
            if ("fail".equals(registerConfig.getServerLists())) {
                throw new IllegalStateException("initialization failed");
            }
            config = registerConfig;
            initCount++;
        }

        @Override
        public void persistInstance(final InstanceEntity instance) {
        }
    }
}
