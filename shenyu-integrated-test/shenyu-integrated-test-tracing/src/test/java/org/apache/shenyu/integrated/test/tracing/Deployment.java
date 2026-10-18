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

package org.apache.shenyu.integrated.test.tracing;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.TimeUnit;

/**
 * The docker deployment of the integrated test.
 *
 * <p>The tests of the sampler configure the gateway after the deployment is started, so the gateway
 * of the compose file is recreated with an override of the compose file instead of shipping one
 * gateway per sampler. The override only adds the tracing settings to the environment of the
 * gateway, everything else stays what the compose file declares.</p>
 */
final class Deployment {

    static final String GATEWAY_CONTAINER = "shenyu-integrated-test-tracing";

    static final String COLLECTOR_CONTAINER = "shenyu-otel-collector";

    private static final Logger LOG = LoggerFactory.getLogger(Deployment.class);

    private static final String COMPOSE_FILE = "docker-compose.yml";

    private static final Path OVERRIDE_FILE = Paths.get("target", "tracing-sampler-override.yml");

    private static final String SAMPLER_PROPERTY = "shenyu.tracing.sampler";

    private static final String SAMPLE_RATIO_PROPERTY = "shenyu.tracing.sample-ratio";

    private static final long COMMAND_TIMEOUT_SECONDS = 180L;

    private Deployment() {
    }

    /**
     * Recreates the gateway with a sampler, the deployment of the compose file is restored by
     * {@link #restoreGateway()} afterwards.
     *
     * @param sampler the sampler of the gateway
     * @throws IOException while the override of the compose file cannot be written
     * @throws InterruptedException while a command of the deployment is interrupted
     */
    static void restartGatewayWithSampler(final String sampler) throws IOException, InterruptedException {
        restartGatewayWithSampler(sampler, null);
    }

    /**
     * Recreates the gateway with a sampler and its sample ratio.
     *
     * @param sampler the sampler of the gateway
     * @param sampleRatio the sample ratio, null for a sampler which takes no ratio
     * @throws IOException while the override of the compose file cannot be written
     * @throws InterruptedException while a command of the deployment is interrupted
     */
    static void restartGatewayWithSampler(final String sampler, final Double sampleRatio)
            throws IOException, InterruptedException {
        final StringBuilder override = new StringBuilder()
                .append("services:").append(System.lineSeparator())
                .append("  ").append(GATEWAY_CONTAINER).append(':').append(System.lineSeparator())
                .append("    environment:").append(System.lineSeparator())
                .append("      - ").append(SAMPLER_PROPERTY).append('=').append(sampler).append(System.lineSeparator());
        if (Objects.nonNull(sampleRatio)) {
            override.append("      - ").append(SAMPLE_RATIO_PROPERTY).append('=').append(sampleRatio)
                    .append(System.lineSeparator());
        }
        Files.createDirectories(OVERRIDE_FILE.toAbsolutePath().getParent());
        Files.writeString(OVERRIDE_FILE, override.toString(), StandardCharsets.UTF_8);
        composeUpGateway(OVERRIDE_FILE.toString());
    }

    /**
     * Restores the gateway of the compose file, so that a deployment is left the way the compose
     * file declares it and the tests of the next run find the default sampler.
     *
     * @throws IOException while a command of the deployment cannot be executed
     * @throws InterruptedException while a command of the deployment is interrupted
     */
    static void restoreGateway() throws IOException, InterruptedException {
        composeUpGateway(null);
    }

    /**
     * Stops the collector, which is what an unreachable exporter looks like to the gateway.
     *
     * @throws IOException while a command of the deployment cannot be executed
     * @throws InterruptedException while a command of the deployment is interrupted
     */
    static void stopCollector() throws IOException, InterruptedException {
        run(Arrays.asList("docker", "stop", COLLECTOR_CONTAINER));
    }

    /**
     * Starts the collector again.
     *
     * @throws IOException while a command of the deployment cannot be executed
     * @throws InterruptedException while a command of the deployment is interrupted
     */
    static void startCollector() throws IOException, InterruptedException {
        run(Arrays.asList("docker", "start", COLLECTOR_CONTAINER));
    }

    /**
     * Acquires the log of the collector, which reports every span it accepted.
     *
     * @return the log of the collector
     * @throws IOException while a command of the deployment cannot be executed
     * @throws InterruptedException while a command of the deployment is interrupted
     */
    static String collectorLogs() throws IOException, InterruptedException {
        return logs(COLLECTOR_CONTAINER);
    }

    /**
     * Acquires the log of the gateway.
     *
     * @return the log of the gateway
     * @throws IOException while a command of the deployment cannot be executed
     * @throws InterruptedException while a command of the deployment is interrupted
     */
    static String gatewayLogs() throws IOException, InterruptedException {
        return logs(GATEWAY_CONTAINER);
    }

    private static String logs(final String container) throws IOException, InterruptedException {
        return run(Arrays.asList("docker", "logs", "--tail", "20000", container));
    }

    private static void composeUpGateway(final String overrideFile) throws IOException, InterruptedException {
        final List<String> command = new ArrayList<>();
        command.addAll(Arrays.asList("docker", "compose", "-f", COMPOSE_FILE));
        if (Objects.nonNull(overrideFile)) {
            command.addAll(Arrays.asList("-f", overrideFile));
        }
        command.addAll(Arrays.asList("up", "-d", "--force-recreate", "--no-deps", GATEWAY_CONTAINER));
        run(command);
    }

    private static String run(final List<String> command) throws IOException, InterruptedException {
        final Process process = new ProcessBuilder(command).redirectErrorStream(true).start();
        // the output is drained by a thread of its own: a log which is longer than the buffer of the
        // pipe would block the child until the test reads it, while reading until the end of the
        // stream would block the test until the child ends, so neither side may depend on the other
        final ByteArrayOutputStream output = new ByteArrayOutputStream();
        final Thread drain = new Thread(() -> {
            try (InputStream stream = process.getInputStream()) {
                stream.transferTo(output);
            } catch (IOException ex) {
                LOG.warn("the output of {} cannot be read: {}", String.join(" ", command), ex.getMessage());
            }
        }, "deployment-command-output");
        drain.setDaemon(true);
        drain.start();
        final boolean finished;
        try {
            finished = process.waitFor(COMMAND_TIMEOUT_SECONDS, TimeUnit.SECONDS);
            if (!finished) {
                process.destroyForcibly();
            }
        } finally {
            // the drain of a finished command ends with the end of its output, while a command which
            // is killed may leave a process behind which holds the pipe, and the collected output is
            // then incomplete
            drain.join(TimeUnit.SECONDS.toMillis(5L));
            if (drain.isAlive()) {
                LOG.warn("the output of {} is still being read, the reported output may be truncated",
                        String.join(" ", command));
            }
        }
        final String collected = output.toString(StandardCharsets.UTF_8);
        if (!finished) {
            throw new IllegalStateException(String.join(" ", command) + " did not finish in "
                    + COMMAND_TIMEOUT_SECONDS + "s: " + collected);
        }
        if (0 != process.exitValue()) {
            throw new IllegalStateException(String.join(" ", command) + " failed: " + collected);
        }
        return collected;
    }
}
