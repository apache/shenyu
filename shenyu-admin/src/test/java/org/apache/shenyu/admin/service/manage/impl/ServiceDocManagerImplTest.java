package org.apache.shenyu.admin.service.manage.impl;

import org.apache.shenyu.admin.model.bean.UpstreamInstance;
import org.apache.shenyu.admin.service.manager.impl.ServiceDocManagerImpl;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.lang.reflect.Field;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.mock;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class ServiceDocManagerImplTest {

    @InjectMocks
    private ServiceDocManagerImpl serviceDocManager;

    @Test
    public void testPullApiDocument(){
        Set<UpstreamInstance> currentServices = mock(HashSet.class);
        serviceDocManager.pullApiDocument(currentServices);
        verify(currentServices).forEach(any());
    }

}
