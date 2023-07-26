package org.apache.shenyu.register.client.apollo;

import static org.junit.Assert.assertEquals;

import org.junit.Test;

public class ApolloConfigTest {
    /**
     * Methods under test.
     *
     * <ul>
     *   <li>default or parameterless constructor of {@link ApolloConfig}
     *   <li>{@link ApolloConfig#setAppId(String)}
     *   <li>{@link ApolloConfig#setClusterName(String)}
     *   <li>{@link ApolloConfig#setEnv(String)}
     *   <li>{@link ApolloConfig#setNamespace(String)}
     *   <li>{@link ApolloConfig#setOperator(String)}
     *   <li>{@link ApolloConfig#setPortalUrl(String)}
     *   <li>{@link ApolloConfig#setToken(String)}
     *   <li>{@link ApolloConfig#getAppId()}
     *   <li>{@link ApolloConfig#getClusterName()}
     *   <li>{@link ApolloConfig#getEnv()}
     *   <li>{@link ApolloConfig#getNamespace()}
     *   <li>{@link ApolloConfig#getOperator()}
     *   <li>{@link ApolloConfig#getPortalUrl()}
     *   <li>{@link ApolloConfig#getToken()}
     * </ul>
     */
    @Test
    public void testConstructor() {
        ApolloConfig actualApolloConfig = new ApolloConfig();
        actualApolloConfig.setAppId("42");
        actualApolloConfig.setClusterName("Cluster Name");
        actualApolloConfig.setEnv("Env");
        actualApolloConfig.setNamespace("Namespace");
        actualApolloConfig.setOperator("Operator");
        actualApolloConfig.setPortalUrl("https://localhost:8080");
        actualApolloConfig.setToken("ABC123");
        assertEquals("42", actualApolloConfig.getAppId());
        assertEquals("Cluster Name", actualApolloConfig.getClusterName());
        assertEquals("Env", actualApolloConfig.getEnv());
        assertEquals("Namespace", actualApolloConfig.getNamespace());
        assertEquals("Operator", actualApolloConfig.getOperator());
        assertEquals("https://localhost:8080", actualApolloConfig.getPortalUrl());
        assertEquals("ABC123", actualApolloConfig.getToken());
    }
}

