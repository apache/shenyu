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

package org.dromara.soul.admin.utils;

import com.auth0.jwt.JWT;
import com.auth0.jwt.JWTVerifier;
import com.auth0.jwt.algorithms.Algorithm;
import com.auth0.jwt.exceptions.JWTCreationException;
import com.auth0.jwt.exceptions.JWTVerificationException;
import com.auth0.jwt.interfaces.DecodedJWT;
import lombok.Data;
import lombok.experimental.UtilityClass;
import lombok.extern.slf4j.Slf4j;
import org.apache.shiro.util.StringUtils;
import org.dromara.soul.admin.config.properties.JwtProperties;
import org.dromara.soul.admin.spring.SpringBeanUtils;

import java.time.LocalDate;
import java.time.ZoneId;
import java.util.Date;
import java.util.Optional;

/**
 * JWT tools.
 *
 * @author YuI
 **/
@UtilityClass
@Slf4j
@Data
public final class JwtUtils {

    /**
     * user id.
     */
    private static String userId;

    /**
     * get user id.
     *
     * @return userId {@link String}
     */
    public static String getUserId() {
        return userId;
    }

    /**
     * according to token to set userid.
     *
     * @param token token
     */
    public static void setUserId(final String token) {
        DecodedJWT jwt = verifierToken(token);
        if (Optional.ofNullable(jwt).isPresent()) {
            userId = jwt.getId();
        }
    }

    /**
     * according to token to get issuer.
     *
     * @param token token
     * @return Issuer {@link String}
     */
    public static String getIssuer(final String token) {
        DecodedJWT jwt = verifierToken(token);
        return Optional.ofNullable(jwt).map(DecodedJWT::getIssuer).orElse("");
    }

    /**
     * according to token to get issuer date.
     *
     * @param token token
     * @return issuer date {@link LocalDate}
     */
    public static LocalDate getIssuerDate(final String token) {
        DecodedJWT jwt = verifierToken(token);
        if (jwt != null) {
            Date date = jwt.getIssuedAt();
            return date.toInstant().atZone(ZoneId.systemDefault()).toLocalDate();
        }
        return null;
    }

    /**
     * generate jwt token.
     *
     * @param userName login's userName
     * @param userId   login's userId
     * @return token
     */
    public static String generateToken(final String userName, final String userId) {
        try {
            return JWT.create().withJWTId(userId).withIssuer(userName) .withIssuedAt(new Date()).sign(generateAlgorithm());
        } catch (IllegalArgumentException | JWTCreationException e) {
            log.error("JWTToken generate fail ", e);
        }
        return StringUtils.EMPTY_STRING;
    }

    private static DecodedJWT verifierToken(final String token) {
        DecodedJWT jwt = null;
        try {
            JWTVerifier verifier = JWT.require(generateAlgorithm()).build();
            jwt = verifier.verify(token);
        } catch (JWTVerificationException e) {
            log.info("jwt decode fail, token: {} ", token, e);
        }
        return jwt;
    }

    private static Algorithm generateAlgorithm() {
        JwtProperties properties = SpringBeanUtils.getInstance().getBean(JwtProperties.class);
        return Algorithm.HMAC256(properties.getKey());
    }
}
