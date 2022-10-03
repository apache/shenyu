package org.apache.shenyu.plugin.casdoor.config;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class CasdoorConfigTest {

    @Test
    public void CasdoorConfig(){
        CasdoorConfig casdoorConfig = new CasdoorConfig("a","b","c","d","e","f");
        assertEquals("a",casdoorConfig.getEndpoint());
        assertEquals("b",casdoorConfig.getClientId());
        assertEquals("c",casdoorConfig.getClientSecret());
        assertEquals("d",casdoorConfig.getCertificate());
        assertEquals("e",casdoorConfig.getOrganizationName());
        assertEquals("f",casdoorConfig.getApplicationName());

        CasdoorConfig casdoorConfig1 = new CasdoorConfig();
        casdoorConfig1.setEndpoint("a");
        casdoorConfig1.setClientId("b");
        casdoorConfig1.setClientSecret("c");
        casdoorConfig1.setCertificate("d");
        casdoorConfig1.setOrganizationName("e");
        casdoorConfig1.setApplicationName("f");
        assertEquals("a",casdoorConfig1.getEndpoint());
        assertEquals("b",casdoorConfig1.getClientId());
        assertEquals("c",casdoorConfig1.getClientSecret());
        assertEquals("d",casdoorConfig1.getCertificate());
        assertEquals("e",casdoorConfig1.getOrganizationName());
        assertEquals("f",casdoorConfig1.getApplicationName());
        assertEquals("authConfig{endpoint='a', clientId='b', clientSecret='c', certificate='d', organizationName='e', applicationName='f'}",casdoorConfig1.toString());
    }
}