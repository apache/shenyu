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

package org.apache.shenyu.register.client.http.utils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * RuntimeUtils.
 */
public final class RuntimeUtils {

    private static final Logger LOGGER = LoggerFactory.getLogger(RuntimeUtils.class);

    private static final Pattern PID_PATTERN = Pattern.compile("\\d+");

    private RuntimeUtils() {
    }

    /**
     * Check whether the port is listening to by other process.
     *
     * @param port the port
     * @return boolean yes or no
     */
    public static boolean listenByOther(final int port) {
        Optional<String> optionalPid = getPortOwner(port);
        if (optionalPid.isPresent() && !optionalPid.get().equals(SystemUtils.getCurrentPID())) {
            LOGGER.warn("PID {} is listening on port {}.", optionalPid.get(), port);
            return true;
        } else {
            return false;
        }
    }

    /**
     * Get the PID of the listening port.
     *
     * @param port the port
     * @return Optional PID
     */
    public static Optional<String> getPortOwner(final int port) {
        if (port <= 0) {
            return Optional.empty();
        }

        Process process;
        try {
            process = Runtime.getRuntime().exec(new String[] {"netstat", getNestatOptions()});
        } catch (Exception e) {
            LOGGER.warn("exec netstat fail. {}", e.getMessage());
            return Optional.empty();
        }

        try (BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(process.getInputStream()))) {
            String line;
            while ((line = bufferedReader.readLine()) != null) {
                if (!line.contains("LISTEN") || !line.contains(":" + port + " ")) {
                    continue;
                }
                String[] fields = line.trim().split(" ");
                Matcher matcher = PID_PATTERN.matcher(fields[fields.length - 1]);
                if (matcher.find()) {
                    return Optional.of(matcher.group(0));
                }
            }
        } catch (Exception e) {
            LOGGER.warn("get netstat input stream fail. {}", e.getMessage());
        } finally {
            if (process != null) {
                process.destroy();
            }
        }

        return Optional.empty();
    }

    private static String getNestatOptions() {
        return SystemUtils.isWindows() ? "-ano" : "-anp";
    }
}
