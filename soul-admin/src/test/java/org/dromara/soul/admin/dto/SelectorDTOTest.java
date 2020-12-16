package org.dromara.soul.admin.dto;

import org.assertj.core.util.Lists;
import org.junit.Before;
import org.junit.Test;
import org.junit.jupiter.api.Assertions;

/**
 * Test case for {@link SelectorDTO}.
 *
 * @author Jiang Jining
 */
public final class SelectorDTOTest {
    
    private SelectorDTO selectorDTOBuilder;
    private SelectorDTO selectorDTOConstructor;
    
    @Before
    public void initSelectorDTO() {
        selectorDTOBuilder = SelectorDTO.builder().continued(false)
                .id("2ec327a4478a4c6287b7b1a7b658c28c")
                .handle("Test handle").enabled(false).sort(3)
                .matchMode(1).loged(false).name("Test selector name")
                .type(2).pluginId("49753686ec2d4f80b77fe3d287a2f11a")
                .selectorConditions(Lists.emptyList()).build();
        selectorDTOConstructor = new SelectorDTO();
        selectorDTOConstructor.setId("2ec327a4478a4c6287b7b1a7b658c28c");
    }
    
    @Test
    public void testSelectorDTO() {
        Assertions.assertNotNull(selectorDTOBuilder);
        Assertions.assertFalse(selectorDTOBuilder.getContinued());
        Assertions.assertFalse(selectorDTOBuilder.getEnabled());
        Assertions.assertFalse(selectorDTOBuilder.getLoged());
        Assertions.assertEquals(selectorDTOBuilder.getId(), "2ec327a4478a4c6287b7b1a7b658c28c");
        Assertions.assertEquals(selectorDTOBuilder.getType(), 2);
        Assertions.assertEquals(selectorDTOBuilder.getName(), "Test selector name");
        Assertions.assertEquals(selectorDTOBuilder.getPluginId(), "49753686ec2d4f80b77fe3d287a2f11a");
        Assertions.assertEquals(selectorDTOBuilder.getSort(), 3);
        Assertions.assertEquals(selectorDTOBuilder.getHandle(), "Test handle");
        Assertions.assertEquals(selectorDTOBuilder.getMatchMode(), 1);
        Assertions.assertEquals(selectorDTOBuilder.getSelectorConditions(), Lists.emptyList());
        selectorDTOBuilder.setEnabled(true);
        selectorDTOBuilder.setContinued(true);
        selectorDTOBuilder.setLoged(true);
        Assertions.assertTrue(selectorDTOBuilder.getContinued());
        Assertions.assertTrue(selectorDTOBuilder.getEnabled());
        Assertions.assertTrue(selectorDTOBuilder.getLoged());
        Assertions.assertEquals(selectorDTOConstructor.getId(), "2ec327a4478a4c6287b7b1a7b658c28c");
    }
}