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

package org.apache.shenyu.admin.service;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.utils.DigestUtils;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.stereotype.Service;

import java.util.regex.Pattern;

/**
 * Central password storage policy for dashboard users.
 */
@Service
public class PasswordHashService {

    private static final Pattern BCRYPT_PATTERN = Pattern.compile("^\\$2[aby]\\$\\d{2}\\$[./A-Za-z0-9]{53}$");

    private static final Pattern LEGACY_SHA512_PATTERN = Pattern.compile("^[A-Fa-f0-9]{128}$");

    private static final int BCRYPT_STRENGTH = 12;

    private final BCryptPasswordEncoder passwordEncoder = new BCryptPasswordEncoder(BCRYPT_STRENGTH);

    /**
     * Encode a raw dashboard user password for storage.
     * Returns the input unchanged if it is blank.
     *
     * @param requestPassword raw password
     * @return bcrypt encoded password
     */
    public String encode(final String requestPassword) {
        if (StringUtils.isBlank(requestPassword)) {
            return requestPassword;
        }
        return passwordEncoder.encode(requestPassword);
    }

    /**
     * Verify a raw password against a bcrypt hash.
     *
     * @param requestPassword raw password
     * @param storedPasswordHash stored password hash
     * @return true when the password matches
     */
    public boolean matches(final String requestPassword, final String storedPasswordHash) {
        if (!isBcryptHash(storedPasswordHash)) {
            return false;
        }
        return passwordEncoder.matches(requestPassword, storedPasswordHash);
    }

    /**
     * Determine whether a stored password uses bcrypt format.
     *
     * @param storedPasswordHash stored password hash
     * @return true when the stored password is bcrypt
     */
    public boolean isBcryptHash(final String storedPasswordHash) {
        return StringUtils.isNotBlank(storedPasswordHash) && BCRYPT_PATTERN.matcher(storedPasswordHash).matches();
    }

    /**
     * Verify a raw password against the legacy SHA-512 hex format.
     *
     * @param requestPassword raw password
     * @param storedPasswordHash stored password hash
     * @return true when the password matches the legacy hash
     */
    public boolean matchesLegacySha512(final String requestPassword, final String storedPasswordHash) {
        return isLegacySha512Hash(storedPasswordHash)
                && StringUtils.equals(DigestUtils.sha512Hex(requestPassword), storedPasswordHash);
    }

    /**
     * Determine whether a stored password uses the legacy SHA-512 hex format.
     *
     * @param storedPasswordHash stored password hash
     * @return true when the stored password is a legacy SHA-512 hex hash
     */
    public boolean isLegacySha512Hash(final String storedPasswordHash) {
        return StringUtils.isNotBlank(storedPasswordHash) && LEGACY_SHA512_PATTERN.matcher(storedPasswordHash).matches();
    }
}
