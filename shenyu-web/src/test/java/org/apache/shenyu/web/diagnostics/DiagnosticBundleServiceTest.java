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

package org.apache.shenyu.web.diagnostics;

import org.junit.jupiter.api.Test;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for {@link DiagnosticBundleService}.
 */
public final class DiagnosticBundleServiceTest {

    @Test
    public void failingCollectorIsIsolatedAndDoesNotLeakItsMessage() {
        DiagnosticProperties properties = new DiagnosticProperties();
        DiagnosticBundleService service = service(properties, new FixedCollector("runtime", "{}"), new ThrowingCollector("bad"));
        List<DiagnosticSection> sections = service.collect().getSections();
        assertEquals(SectionStatus.OK, sections.get(0).getStatus());
        assertEquals(SectionStatus.FAILED, sections.get(1).getStatus());
        assertEquals("IllegalStateException", sections.get(1).getReason());
        assertFalse(sections.get(1).getReason().contains("boom-secret"));
    }

    @Test
    public void slowCollectorTimesOutIndependently() {
        DiagnosticProperties properties = new DiagnosticProperties();
        properties.setSectionTimeoutMs(50L);
        DiagnosticBundleService service = service(properties, new FixedCollector("runtime", "{}"), new SleepingCollector("slow", 5000L));
        List<DiagnosticSection> sections = service.collect().getSections();
        assertEquals(SectionStatus.OK, sections.get(0).getStatus());
        assertEquals(SectionStatus.FAILED, sections.get(1).getStatus());
        assertEquals("section timeout", sections.get(1).getReason());
    }

    @Test
    public void exhaustedTotalBudgetSkipsEverySection() {
        DiagnosticProperties properties = new DiagnosticProperties();
        properties.setTotalTimeoutMs(0L);
        DiagnosticBundleService service = service(properties, new FixedCollector("runtime", "{}"), new FixedCollector("plugins", "[]"));
        List<DiagnosticSection> sections = service.collect().getSections();
        assertEquals(2, sections.size());
        assertEquals(SectionStatus.SKIPPED, sections.get(0).getStatus());
        assertEquals(SectionStatus.SKIPPED, sections.get(1).getStatus());
        assertEquals("total budget exhausted", sections.get(0).getReason());
    }

    @Test
    public void oversizedSectionIsTruncated() {
        DiagnosticProperties properties = new DiagnosticProperties();
        properties.setMaxSectionBytes(8);
        DiagnosticBundleService service = service(properties, new FixedCollector("runtime", "x".repeat(200)));
        DiagnosticSection section = service.collect().getSections().get(0);
        assertEquals(SectionStatus.TRUNCATED, section.getStatus());
        assertTrue(section.getSizeBytes() <= 8);
    }

    @Test
    public void healthyBundleProducesArchive() throws IOException {
        DiagnosticProperties properties = new DiagnosticProperties();
        DiagnosticBundleService service = service(properties, new FixedCollector("runtime", "{}"), new FixedCollector("plugins", "[]"));
        byte[] archive = service.bundle();
        assertTrue(archive.length > 0);
        assertTrue(readEntry(archive, "manifest.json").contains("formatVersion"));
    }

    @Test
    public void concurrentLimitDegradesToOverloadBundle() throws Exception {
        DiagnosticProperties properties = new DiagnosticProperties();
        properties.setMaxConcurrentJobs(1);
        properties.setSectionTimeoutMs(10000L);
        BlockingCollector blocking = new BlockingCollector("runtime");
        DiagnosticBundleService service = service(properties, blocking);
        AtomicReference<byte[]> first = new AtomicReference<>();
        Thread worker = new Thread(() -> first.set(service.bundle()));
        worker.start();
        assertTrue(blocking.started.await(5L, TimeUnit.SECONDS));
        byte[] overload = service.bundle();
        assertTrue(readEntry(overload, "manifest.json").contains("concurrent collection limit reached"));
        blocking.release.countDown();
        worker.join(TimeUnit.SECONDS.toMillis(10L));
        assertTrue(Objects.nonNull(first.get()) && first.get().length > 0);
    }

    private DiagnosticBundleService service(final DiagnosticProperties properties, final SectionCollector... collectors) {
        return new DiagnosticBundleService(List.of(collectors), properties, new DiagnosticBundleWriter(properties));
    }

    private String readEntry(final byte[] archive, final String target) throws IOException {
        try (ZipInputStream zip = new ZipInputStream(new ByteArrayInputStream(archive), StandardCharsets.UTF_8)) {
            ZipEntry entry = zip.getNextEntry();
            while (Objects.nonNull(entry)) {
                if (entry.getName().equals(target)) {
                    return new String(zip.readAllBytes(), StandardCharsets.UTF_8);
                }
                entry = zip.getNextEntry();
            }
        }
        return "";
    }

    private static final class FixedCollector implements SectionCollector {

        private final String name;

        private final String content;

        FixedCollector(final String name, final String content) {
            this.name = name;
            this.content = content;
        }

        @Override
        public String name() {
            return name;
        }

        @Override
        public DiagnosticSection collect(final DiagnosticBudget budget) {
            return DiagnosticSection.ok(name, System.currentTimeMillis(), 0L, content);
        }
    }

    private static final class ThrowingCollector implements SectionCollector {

        private final String name;

        ThrowingCollector(final String name) {
            this.name = name;
        }

        @Override
        public String name() {
            return name;
        }

        @Override
        public DiagnosticSection collect(final DiagnosticBudget budget) {
            throw new IllegalStateException("boom-secret");
        }
    }

    private static final class SleepingCollector implements SectionCollector {

        private final String name;

        private final long sleepMs;

        SleepingCollector(final String name, final long sleepMs) {
            this.name = name;
            this.sleepMs = sleepMs;
        }

        @Override
        public String name() {
            return name;
        }

        @Override
        public DiagnosticSection collect(final DiagnosticBudget budget) {
            try {
                Thread.sleep(sleepMs);
            } catch (InterruptedException ex) {
                Thread.currentThread().interrupt();
            }
            return DiagnosticSection.ok(name, System.currentTimeMillis(), 0L, "{}");
        }
    }

    private static final class BlockingCollector implements SectionCollector {

        private final CountDownLatch started = new CountDownLatch(1);

        private final CountDownLatch release = new CountDownLatch(1);

        private final String name;

        BlockingCollector(final String name) {
            this.name = name;
        }

        @Override
        public String name() {
            return name;
        }

        @Override
        public DiagnosticSection collect(final DiagnosticBudget budget) {
            started.countDown();
            try {
                release.await(10L, TimeUnit.SECONDS);
            } catch (InterruptedException ex) {
                Thread.currentThread().interrupt();
            }
            return DiagnosticSection.ok(name, System.currentTimeMillis(), 0L, "{}");
        }
    }
}
