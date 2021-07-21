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

package org.apache.shenyu.admin.utils;

import com.auth0.jwt.JWT;
import com.auth0.jwt.JWTVerifier;
import com.auth0.jwt.algorithms.Algorithm;
import com.auth0.jwt.exceptions.JWTCreationException;
import com.auth0.jwt.exceptions.JWTVerificationException;
import com.auth0.jwt.interfaces.DecodedJWT;
import lombok.Data;
import lombok.experimental.UtilityClass;
import lombok.extern.slf4j.Slf4j;
import org.apache.shenyu.admin.model.custom.UserInfo;
import org.apache.shiro.SecurityUtils;
import org.apache.shiro.util.StringUtils;

import java.util.Date;
import java.util.Optional;

/**
 * JWT tools.
 */
@UtilityClass
@Slf4j
@Data
public final class JwtUtils {

    private static final long TOKEN_EXPIRE_SECONDS = 24 * 60 * 60 * 1000L;

    /**
     * according to token to get isUserInfo.
     *
     * @return UserInfo {@link UserInfo}
     */
    public static UserInfo getUserInfo() {
        return (UserInfo) SecurityUtils.getSubject().getPrincipal();
    }

    /**
     * according to token to get issuer.
     *
     * @param token token
     * @return Issuer {@link String}
     */
    public static String getIssuer(final String token) {
        DecodedJWT jwt = JWT.decode(token);
        return Optional.ofNullable(jwt).map(item -> item.getClaim("userName").asString()).orElse("");
    }

    /**
     * generate jwt token.
     *
     * @param userName login's userName
     * @param key secretKey
     * @return token
     */
    public static String generateToken(final String userName, final String key) {
        return generateToken(userName, key, null);
    }

    /**
     * generate jwt token.
     *
     * @param userName login's userName
     * @param key secretKey
     * @param expireSeconds expireSeconds
     * @return token
     */
    public static String generateToken(final String userName, final String key, final Long expireSeconds) {
        try {
            return JWT.create()
                    .withClaim("userName", userName)
                    .withExpiresAt(new Date(System.currentTimeMillis() + Optional.ofNullable(expireSeconds).orElse(TOKEN_EXPIRE_SECONDS)))
                    .sign(Algorithm.HMAC256(key));
        } catch (IllegalArgumentException | JWTCreationException e) {
            log.error("JWTToken generate fail ", e);
        }
        return StringUtils.EMPTY_STRING;
    }

    public static boolean verifyToken(final String token, final String key) {
        try {
            JWTVerifier verifier = JWT.require(Algorithm.HMAC256(key)).build();
            verifier.verify(token);
            return true;
        } catch (JWTVerificationException e) {
            log.info("jwt decode fail, token: {} ", token, e);
        }
        return false;
    }
}
