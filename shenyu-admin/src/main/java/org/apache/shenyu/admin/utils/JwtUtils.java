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
import org.apache.shenyu.admin.config.properties.JwtProperties;
import org.apache.shenyu.admin.model.custom.UserInfo;
import org.apache.shenyu.admin.spring.SpringBeanUtils;
import org.apache.shiro.SecurityUtils;
import org.apache.shiro.util.StringUtils;

import java.time.LocalDate;
import java.time.ZoneId;
import java.util.Date;
import java.util.Optional;

/**
 * JWT tools.
 */
@UtilityClass
@Slf4j
@Data
public final class JwtUtils {

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
     * according to token to get issuer date.
     *
     * @param token token
     * @return issuer date {@link LocalDate}
     */
    public static LocalDate getIssuerDate(final String token) {
        DecodedJWT jwt = verifierToken(token);
        if (jwt == null) {
            return null;
        }
        Date date = jwt.getIssuedAt();
        return Optional.ofNullable(date)
                .map(it -> it.toInstant().atZone(ZoneId.systemDefault()).toLocalDate())
                .orElse(null);
    }

    /**
     * generate jwt token.
     *
     * @param userName login's userName
     * @return token
     */
    public static String generateToken(final String userName) {
        try {
            return JWT.create().withClaim("userName", userName).withExpiresAt(new Date()).sign(generateAlgorithm());
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
