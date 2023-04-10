package org.apache.shenyu.common.enums;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class ApiSourceEnumTest {

    @Test
    public void testGetName() {
        assertEquals("swagger", ApiSourceEnum.SWAGGER.getName());
        assertEquals("annotation_generation", ApiSourceEnum.ANNOTATION_GENERATION.getName());
        assertEquals("create_manually", ApiSourceEnum.CREATE_MANUALLY.getName());
        assertEquals("import_swagger", ApiSourceEnum.IMPORT_SWAGGER.getName());
        assertEquals("import_yapi", ApiSourceEnum.IMPORT_YAPI.getName());
    }

    @Test
    public void testGetValue() {
        assertEquals(0, ApiSourceEnum.SWAGGER.getValue());
        assertEquals(1, ApiSourceEnum.ANNOTATION_GENERATION.getValue());
        assertEquals(2, ApiSourceEnum.CREATE_MANUALLY.getValue());
        assertEquals(3, ApiSourceEnum.IMPORT_SWAGGER.getValue());
        assertEquals(4, ApiSourceEnum.IMPORT_YAPI.getValue());
    }
}
