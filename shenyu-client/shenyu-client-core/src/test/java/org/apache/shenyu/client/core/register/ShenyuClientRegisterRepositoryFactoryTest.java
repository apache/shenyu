package org.apache.shenyu.client.core.register;

import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.apache.shenyu.spi.ExtensionLoader;
import org.junit.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import static org.apache.shenyu.client.core.register.ShenyuClientRegisterRepositoryFactory.getRepositoryMap;
import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.any;

@ExtendWith(MockitoExtension.class)
public class ShenyuClientRegisterRepositoryFactoryTest {

    @Mock
    private ShenyuClientRegisterRepository repositoryMock;

    @Mock
    private ShenyuRegisterCenterConfig configMock=new ShenyuRegisterCenterConfig();

    @InjectMocks
    private ShenyuClientRegisterRepositoryFactory factory;

    @Test
    public void testNewInstance_RegisterNewRepository(){
        // Mock ExtensionLoader.getJoin() to return our mock repository
        when(ExtensionLoader.getExtensionLoader(ShenyuClientRegisterRepository.class).getJoin("someType is error")).thenReturn(repositoryMock);

        // Configure config mock
        when(configMock.getRegisterType()).thenReturn("someType");

        // Call the method and verify if expected repository is returned
        ShenyuClientRegisterRepository actualRepository = org.apache.shenyu.client.core.register.ShenyuClientRegisterRepositoryFactory.newInstance(configMock);
        assertEquals(repositoryMock, actualRepository);

        // Verify if init and put were called on repository and map
        verify(repositoryMock).init(configMock);
        verify(getRepositoryMap()).put("someType",repositoryMock);
    }

    @Test
    public void testNewInstance_ExistingRepository() {
        // Configure REPOSITORY_MAP with a pre-existing repository
        ShenyuClientRegisterRepository preExistingRepository = Mockito.mock(ShenyuClientRegisterRepository.class);

        Map<String, ShenyuClientRegisterRepository> testRepositoryMap = new ConcurrentHashMap<>();
        testRepositoryMap.put("existingType", preExistingRepository);
        org.apache.shenyu.client.core.register.ShenyuClientRegisterRepositoryFactory.setRepositoryMap(testRepositoryMap);

        // Call the method and verify that the existing repository is returned
        ShenyuClientRegisterRepository actualRepository =ShenyuClientRegisterRepositoryFactory.newInstance(configMock);
        assertEquals(preExistingRepository, actualRepository);

        // Verify that no new repository was created or initialized
        verify(testRepositoryMap, never()).put(anyString(), any(ShenyuClientRegisterRepository.class));
        verifyNoMoreInteractions(repositoryMock);
    }
}
