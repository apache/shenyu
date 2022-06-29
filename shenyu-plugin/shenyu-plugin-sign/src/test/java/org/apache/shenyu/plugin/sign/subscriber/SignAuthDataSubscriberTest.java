package org.apache.shenyu.plugin.sign.subscriber;

import org.apache.shenyu.common.dto.AppAuthData;
import org.apache.shenyu.plugin.sign.cache.SignAuthDataCache;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

/**
 * The Shenyu default sign subscriber test.
 */
@ExtendWith(MockitoExtension.class)
public class SignAuthDataSubscriberTest {

    private SignAuthDataSubscriber signAuthDataSubscriber;

    @BeforeEach
    public void setUp() {
        signAuthDataSubscriber = new SignAuthDataSubscriber();
    }

    @Test
    void onSubscribe() {
        AppAuthData appAuthData = new AppAuthData();
        appAuthData.setAppKey("D9FD95F496C9495DB5604222A13C3D08");
        appAuthData.setAppSecret("02D25048AA1E466F8920E68B08E668DE");
        appAuthData.setEnabled(true);
        signAuthDataSubscriber.onSubscribe(appAuthData);
        assertEquals(SignAuthDataCache.getInstance().obtainAuthData("D9FD95F496C9495DB5604222A13C3D08"), appAuthData);
    }

    @Test
    void unSubscribe() {
        AppAuthData appAuthData = new AppAuthData();
        appAuthData.setAppKey("D9FD95F496C9495DB5604222A13C3D08");
        appAuthData.setAppSecret("02D25048AA1E466F8920E68B08E668DE");
        appAuthData.setEnabled(true);
        signAuthDataSubscriber.onSubscribe(appAuthData);
        signAuthDataSubscriber.unSubscribe(appAuthData);
        assertNull(SignAuthDataCache.getInstance().obtainAuthData("D9FD95F496C9495DB5604222A13C3D08"));
    }
}
