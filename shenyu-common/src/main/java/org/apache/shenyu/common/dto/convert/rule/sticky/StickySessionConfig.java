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

package org.apache.shenyu.common.dto.convert.rule.sticky;

import java.util.List;

/**
 * Optional sticky session affinity configuration for HTTP proxying.
 * Composed with the configured load-balancing algorithm: the algorithm
 * selects the target for new sessions, the affinity layer reuses a
 * previously selected target while it stays eligible (#8037).
 */
public class StickySessionConfig {

    /**
     * Default response cookie name.
     */
    public static final String DEFAULT_COOKIE_NAME = "Shenyu-Route";

    /**
     * Default binding lifetime in seconds.
     */
    public static final int DEFAULT_MAX_AGE_SECONDS = 3600;

    /**
     * Whether sticky affinity is enabled for the rule. Disabled rules keep
     * the plain load-balancing behavior with no cookie and no allocation.
     */
    private boolean enabled;

    /**
     * Response cookie name.
     */
    private String cookieName = DEFAULT_COOKIE_NAME;

    /**
     * Binding lifetime in seconds; the cookie expires with the binding.
     */
    private int maxAgeSeconds = DEFAULT_MAX_AGE_SECONDS;

    /**
     * HMAC keys. The first key signs new cookies; every key verifies
     * received ones, so a key can be rotated by appending it (#8037).
     */
    private List<String> secrets;

    /**
     * What happens when the bound target is not eligible: REBIND selects a
     * replacement with the load-balancing algorithm and rebinds, STRICT
     * rejects the request instead of silently changing the session's target.
     */
    private String onUnavailable = "REBIND";

    /**
     * Whether affinity is enabled.
     *
     * @return enabled
     */
    public boolean isEnabled() {
        return enabled;
    }

    /**
     * set enabled.
     *
     * @param enabled enabled
     */
    public void setEnabled(final boolean enabled) {
        this.enabled = enabled;
    }

    /**
     * get cookieName.
     *
     * @return cookieName
     */
    public String getCookieName() {
        return cookieName;
    }

    /**
     * set cookieName.
     *
     * @param cookieName cookieName
     */
    public void setCookieName(final String cookieName) {
        this.cookieName = cookieName;
    }

    /**
     * get maxAgeSeconds.
     *
     * @return maxAgeSeconds
     */
    public int getMaxAgeSeconds() {
        return maxAgeSeconds;
    }

    /**
     * set maxAgeSeconds.
     *
     * @param maxAgeSeconds maxAgeSeconds
     */
    public void setMaxAgeSeconds(final int maxAgeSeconds) {
        this.maxAgeSeconds = maxAgeSeconds;
    }

    /**
     * get secrets.
     *
     * @return secrets
     */
    public List<String> getSecrets() {
        return secrets;
    }

    /**
     * set secrets.
     *
     * @param secrets secrets
     */
    public void setSecrets(final List<String> secrets) {
        this.secrets = secrets;
    }

    /**
     * get onUnavailable.
     *
     * @return onUnavailable
     */
    public String getOnUnavailable() {
        return onUnavailable;
    }

    /**
     * set onUnavailable.
     *
     * @param onUnavailable onUnavailable
     */
    public void setOnUnavailable(final String onUnavailable) {
        this.onUnavailable = onUnavailable;
    }
}
