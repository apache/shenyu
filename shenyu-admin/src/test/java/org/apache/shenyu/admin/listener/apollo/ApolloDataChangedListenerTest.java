package org.apache.shenyu.admin.listener.apollo;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.apache.shenyu.admin.config.properties.ApolloProperties;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

@ContextConfiguration(classes = {ApolloDataChangedListener.class})
@RunWith(SpringJUnit4ClassRunner.class)
public class ApolloDataChangedListenerTest {
    @MockBean
    private ApolloClient apolloClient;

    @Autowired
    private ApolloDataChangedListener apolloDataChangedListener;

    /**
     * Method under test: {@link ApolloDataChangedListener#ApolloDataChangedListener(ApolloClient)}
     */
    @Test
    public void testConstructor() {
        ApolloProperties apolloConfig = new ApolloProperties();
        apolloConfig.setAppId("42");
        apolloConfig.setClusterName("Cluster Name");
        apolloConfig.setEnv("Env");
        apolloConfig.setMeta("Meta");
        apolloConfig.setNamespace("Namespace");
        apolloConfig.setPortalUrl("http://localhost:8080");
        apolloConfig.setToken("ABC123");
        new ApolloDataChangedListener(new ApolloClient(apolloConfig));
    }

    /**
     * Method under test: {@link ApolloDataChangedListener#publishConfig(String, Object)}
     */
    @Test
    public void testPublishConfig() {
        doNothing().when(apolloClient)
                .createOrUpdateItem(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<String>any());
        doNothing().when(apolloClient).publishNamespace(Mockito.<String>any(), Mockito.<String>any());
        apolloDataChangedListener.publishConfig("42", "Data");
        verify(apolloClient).createOrUpdateItem(Mockito.<String>any(), Mockito.<Object>any(), Mockito.<String>any());
        verify(apolloClient).publishNamespace(Mockito.<String>any(), Mockito.<String>any());
    }

    /**
     * Method under test: {@link ApolloDataChangedListener#getConfig(String)}
     */
    @Test
    public void testGetConfig() {
        when(apolloClient.getItemValue(Mockito.<String>any())).thenReturn("42");
        assertEquals("42", apolloDataChangedListener.getConfig("42"));
        verify(apolloClient).getItemValue(Mockito.<String>any());
    }

    /**
     * Method under test: {@link ApolloDataChangedListener#getConfig(String)}
     */
    @Test
    public void testGetConfig2() {
        when(apolloClient.getItemValue(Mockito.<String>any())).thenReturn("");
        assertEquals("{}", apolloDataChangedListener.getConfig("42"));
        verify(apolloClient).getItemValue(Mockito.<String>any());
    }
}

