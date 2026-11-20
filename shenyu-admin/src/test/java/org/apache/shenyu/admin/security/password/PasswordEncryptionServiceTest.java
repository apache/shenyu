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

package org.apache.shenyu.admin.security.password;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import jakarta.validation.Validation;
import jakarta.validation.ValidationException;
import jakarta.validation.Validator;
import jakarta.validation.ValidatorFactory;
import org.apache.shenyu.admin.config.properties.SecretProperties;
import org.apache.shenyu.admin.config.properties.DashboardProperties;
import org.apache.shenyu.admin.config.properties.JwtProperties;
import org.apache.shenyu.admin.mapper.DashboardUserMapper;
import org.apache.shenyu.admin.mapper.RoleMapper;
import org.apache.shenyu.admin.mapper.UserRoleMapper;
import org.apache.shenyu.admin.model.entity.DashboardUserDO;
import org.apache.shenyu.admin.model.dto.LoginDashboardUserDTO;
import org.apache.shenyu.admin.service.NamespaceUserService;
import org.apache.shenyu.admin.service.PasswordHashService;
import org.apache.shenyu.admin.service.impl.DashboardUserServiceImpl;
import org.apache.shenyu.admin.service.publish.UserEventPublisher;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.Test;

import javax.crypto.Cipher;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;
import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.sql.Timestamp;
import java.util.Base64;

import static org.apache.shenyu.admin.security.password.PasswordEnvelopeFixtures.MAPPER;
import static org.apache.shenyu.admin.security.password.PasswordEnvelopeFixtures.encrypt;
import static org.apache.shenyu.admin.security.password.PasswordEnvelopeFixtures.encryptInner;
import static org.apache.shenyu.admin.security.password.PasswordEnvelopeFixtures.metadata;
import static org.junit.jupiter.api.Assertions.assertArrayEquals;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.ArgumentMatchers.any;

/**
 * Real JCA protocol, compatibility and failure-boundary tests.
 */
class PasswordEncryptionServiceTest {

    private static final ValidatorFactory VALIDATOR_FACTORY = Validation.buildDefaultValidatorFactory();

    private static final Validator VALIDATOR = VALIDATOR_FACTORY.getValidator();

    private final SecretProperties legacy = new SecretProperties();

    private final PasswordEncryptionProperties properties = new PasswordEncryptionProperties();

    private final PasswordEncryptionService service = new PasswordEncryptionService(properties, legacy, VALIDATOR);

    @AfterAll
    static void closeValidator() {
        VALIDATOR_FACTORY.close();
    }

    @Test
    void decryptsChineseAndLongLdapPasswordsWithoutRsaLengthRestrictions() throws Exception {
        String password = "中文LDAP密码!".repeat(200);
        ObjectNode login = MAPPER.createObjectNode().put("userName", "ldap-user").put("password", password).put("clientId", "browser-client");
        JsonNode result = MAPPER.readTree(service.decodeRequest(encrypt(service, "login", "", login), "login", ""));
        assertEquals(login, result);
        ObjectNode change = MAPPER.createObjectNode().put("userName", "ldap-user").put("password", "New!Password2").put("oldPassword", password);
        assertEquals(change, MAPPER.readTree(service.decodeRequest(encrypt(service, "change-password", "用户2", change), "change-password", "用户2")));
    }

    @Test
    void advertisesOnlyPublicMetadataAndSanitizedAesFields() throws Exception {
        legacy.setKey("test-key-never-advertised");
        legacy.setIv("test-iv-never-advertised");
        String info = new String(Base64.getDecoder().decode(service.secretInfo()), StandardCharsets.UTF_8);
        JsonNode node = MAPPER.readTree(info);
        assertEquals("", node.path("key").textValue());
        assertEquals("", node.path("iv").textValue());
        assertEquals("RSA-OAEP-256+A256GCM", node.path("encryption").path("algorithm").textValue());
        assertEquals(120, node.path("encryption").path("maxAgeSeconds").intValue());
        assertFalse(info.contains(legacy.getKey()));
        assertFalse(info.contains(legacy.getIv()));
        assertFalse(info.contains("privateKey"));
    }

    @Test
    void rejectsTamperedFieldsAndUnexpectedOrMissingFieldsWithoutLeakingCauses() throws Exception {
        ObjectNode data = MAPPER.createObjectNode().put("userName", "test").put("password", "secret-not-logged");
        ObjectNode original = (ObjectNode) MAPPER.readTree(encrypt(service, "login", "", data));
        for (String field : new String[]{"wrappedKey", "iv", "ciphertext"}) {
            ObjectNode broken = original.deepCopy();
            byte[] bytes = Base64.getDecoder().decode(broken.path(field).textValue());
            bytes[0] ^= 1;
            broken.put(field, Base64.getEncoder().encodeToString(bytes));
            assertSafeFailure(MAPPER.writeValueAsBytes(broken));
            broken.put(field, "not base64!");
            assertSafeFailure(MAPPER.writeValueAsBytes(broken));
        }
        assertSafeFailure(MAPPER.writeValueAsBytes(original.deepCopy().put("keyId", "wrong-key")));
        assertSafeFailure(MAPPER.writeValueAsBytes(original.deepCopy().put("version", 2)));
        assertSafeFailure(MAPPER.writeValueAsBytes(original.deepCopy().put("version", new BigInteger("18446744073709551617"))));
        assertSafeFailure(MAPPER.writeValueAsBytes(original.deepCopy().put("password", "plaintext-must-not-be-accepted")));
        ObjectNode missing = original.deepCopy();
        missing.remove("wrappedKey");
        assertSafeFailure(MAPPER.writeValueAsBytes(missing));
        String duplicate = new String(MAPPER.writeValueAsBytes(original), StandardCharsets.UTF_8).replace("{", "{\"version\":1,");
        assertSafeFailure(duplicate.getBytes(StandardCharsets.UTF_8));
        ObjectNode unpadded = original.deepCopy();
        unpadded.put("wrappedKey", unpadded.get("wrappedKey").textValue().replace("=", ""));
        assertSafeFailure(MAPPER.writeValueAsBytes(unpadded));
        assertSafeFailure((original.toString() + " {}").getBytes(StandardCharsets.UTF_8));
        assertSafeFailure(new byte[PasswordEncryptionService.MAX_BODY_BYTES + 1]);
    }

    @Test
    void bindsCiphertextToPurposeAndDecodedTargetId() throws Exception {
        ObjectNode data = MAPPER.createObjectNode().put("userName", "test").put("password", "Test!Pass2");
        byte[] login = encrypt(service, "login", "", data);
        assertThrows(InvalidPasswordRequestException.class, () -> service.decodeRequest(login, "change-password", "2"));
        byte[] change = encrypt(service, "change-password", "2", data);
        assertThrows(InvalidPasswordRequestException.class, () -> service.decodeRequest(change, "change-password", "3"));
        assertThrows(InvalidPasswordRequestException.class, () -> service.decodeRequest(change, "login", ""));
        assertThrows(InvalidPasswordRequestException.class, () -> service.decodeRequest(change, "unknown-purpose", "2"));
    }

    @Test
    void enforcesAgeFutureSkewAndStrictInnerFields() throws Exception {
        ObjectNode data = MAPPER.createObjectNode().put("userName", "test").put("password", "secret");
        assertSafeFailure(encrypt(service, "login", "", data, System.currentTimeMillis() - 121_000L));
        assertSafeFailure(encrypt(service, "login", "", data, System.currentTimeMillis() + 31_000L));
        ObjectNode inner = MAPPER.createObjectNode().put("issuedAt", "not-a-time");
        inner.set("data", data);
        assertSafeFailure(encryptInner(service, "login", "", inner));
        inner.put("issuedAt", System.currentTimeMillis());
        inner.put("unexpected", true);
        assertSafeFailure(encryptInner(service, "login", "", inner));
        assertSafeFailure(encrypt(service, "login", "", MAPPER.createArrayNode()));
        assertSafeFailure(encrypt(service, "login", "", data.deepCopy().put("extra", "ignored-fields-not-allowed")));
    }

    @Test
    void validatesOriginalLoginBeforeLegacyEncryptionCanHideEmptyPassword() throws Exception {
        legacy.setKey("1234567890123456");
        legacy.setIv("abcdefghijklmnop");
        assertSafeFailure(encrypt(service, "login", "", MAPPER.createObjectNode().put("userName", "test").put("password", "")));
        assertSafeFailure(encrypt(service, "login", "", MAPPER.createObjectNode().put("userName", " ").put("password", "strong-enough-to-pass")));
    }

    @Test
    void bridgesBase64LookingLiteralPasswordExactlyOnceForExistingLoginDecoder() throws Exception {
        legacy.setKey("1234567890123456");
        legacy.setIv("abcdefghijklmnop");
        String password = "MDEyMzQ1Njc4OWFiY2RlZg==";
        ObjectNode data = MAPPER.createObjectNode().put("userName", "test").put("password", password);
        JsonNode decoded = MAPPER.readTree(service.decodeRequest(encrypt(service, "login", "", data), "login", ""));
        Cipher cipher = Cipher.getInstance("AES/CBC/PKCS5Padding");
        cipher.init(Cipher.DECRYPT_MODE, new SecretKeySpec(legacy.getKey().getBytes(StandardCharsets.UTF_8), "AES"),
                new IvParameterSpec(legacy.getIv().getBytes(StandardCharsets.UTF_8)));
        assertEquals(password, new String(cipher.doFinal(Base64.getDecoder().decode(decoded.get("password").textValue())), StandardCharsets.UTF_8));
        DashboardUserMapper users = mock(DashboardUserMapper.class);
        PasswordHashService hashes = mock(PasswordHashService.class);
        when(users.selectByUserName("test")).thenReturn(DashboardUserDO.builder().id("2").userName("test").enabled(true).password("stored-hash")
                .dateCreated(new Timestamp(System.currentTimeMillis())).dateUpdated(new Timestamp(System.currentTimeMillis())).build());
        when(hashes.matches(password, "stored-hash")).thenReturn(true);
        JwtProperties jwt = new JwtProperties();
        jwt.setSecretKey("password-envelope-jwt-test-key");
        jwt.setExpiredSeconds(60L);
        DashboardUserServiceImpl original = new DashboardUserServiceImpl(users, mock(UserRoleMapper.class), mock(RoleMapper.class), null, null,
                jwt, mock(UserEventPublisher.class), new DashboardProperties(), legacy, mock(NamespaceUserService.class), hashes);
        assertEquals("test", original.login("test", decoded.get("password").textValue(), null).getUserName());
        verify(hashes).matches(password, "stored-hash");
    }

    @Test
    void refusesPartialOrInvalidLegacyConfigurationRatherThanPassingAmbiguousPlaintext() throws Exception {
        ObjectNode data = MAPPER.createObjectNode().put("userName", "test").put("password", "literal-password");
        legacy.setKey("1234567890123456");
        assertSafeFailure(encrypt(service, "login", "", data));
        legacy.setIv("bad");
        assertSafeFailure(encrypt(service, "login", "", data));
        legacy.setKey("invalid-length");
        legacy.setIv("abcdefghijklmnop");
        assertSafeFailure(encrypt(service, "login", "", data));
    }

    @Test
    void sharesConfiguredKeyAcrossInstancesAndRejectsUndersizedOrInvalidPrivateKeys() throws Exception {
        KeyPairGenerator generator = KeyPairGenerator.getInstance("RSA");
        generator.initialize(2048);
        KeyPair pair = generator.generateKeyPair();
        properties.setPrivateKey("-----BEGIN PRIVATE KEY-----\n" + Base64.getEncoder().encodeToString(pair.getPrivate().getEncoded()) + "\n-----END PRIVATE KEY-----");
        PasswordEncryptionService first = new PasswordEncryptionService(properties, legacy, VALIDATOR);
        PasswordEncryptionService second = new PasswordEncryptionService(properties, legacy, VALIDATOR);
        assertEquals(metadata(first).get("keyId"), metadata(second).get("keyId"));
        ObjectNode data = MAPPER.createObjectNode().put("userName", "test").put("password", "literal-password");
        assertEquals(data, MAPPER.readTree(second.decodeRequest(encrypt(first, "login", "", data), "login", "")));
        generator.initialize(1024);
        properties.setPrivateKey(Base64.getEncoder().encodeToString(generator.generateKeyPair().getPrivate().getEncoded()));
        assertThrows(IllegalStateException.class, () -> new PasswordEncryptionService(properties, legacy, VALIDATOR));
        properties.setPrivateKey("invalid-key-not-logged");
        IllegalStateException failure = assertThrows(IllegalStateException.class, () -> new PasswordEncryptionService(properties, legacy, VALIDATOR));
        assertNull(failure.getCause());
        assertFalse(failure.getMessage().contains(properties.getPrivateKey()));
    }

    @Test
    void acceptsLegacyJsonOnlyWhenCompatibilityIsExplicitlyEnabled() throws Exception {
        byte[] plain = MAPPER.writeValueAsBytes(MAPPER.createObjectNode().put("userName", "test").put("password", "literal"));
        assertArrayEquals(plain, service.decodeRequest(plain, "login", ""));
        properties.setRequired(true);
        assertSafeFailure(plain);
        assertSafeFailure("null".getBytes(StandardCharsets.UTF_8));
    }

    @Test
    void sanitizesUnexpectedValidationProviderFailures() throws Exception {
        Validator failing = mock(Validator.class);
        when(failing.validate(any(LoginDashboardUserDTO.class))).thenThrow(new ValidationException("fixture-plaintext-must-not-escape"));
        PasswordEncryptionService broken = new PasswordEncryptionService(properties, legacy, failing);
        byte[] request = encrypt(broken, "login", "", MAPPER.createObjectNode().put("userName", "test").put("password", "password"));
        InvalidPasswordRequestException failure = assertThrows(InvalidPasswordRequestException.class, () -> broken.decodeRequest(request, "login", ""));
        assertEquals("Invalid password request", failure.getMessage());
        assertNull(failure.getCause());
        verify(failing).validate(any(LoginDashboardUserDTO.class));
    }

    private void assertSafeFailure(final byte[] request) {
        InvalidPasswordRequestException failure = assertThrows(InvalidPasswordRequestException.class, () -> service.decodeRequest(request, "login", ""));
        assertEquals("Invalid password request", failure.getMessage());
        assertNull(failure.getCause());
    }
}
