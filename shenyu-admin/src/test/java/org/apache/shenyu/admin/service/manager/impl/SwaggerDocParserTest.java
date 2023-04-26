package org.apache.shenyu.admin.service.manager.impl;

import com.google.gson.JsonObject;
import org.apache.shenyu.admin.model.bean.DocInfo;
import org.apache.shenyu.admin.service.manager.impl.SwaggerDocParser;
import org.apache.shenyu.common.utils.GsonUtils;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class SwaggerDocParserTest {

    @InjectMocks
    SwaggerDocParser swaggerDocParser;
    @Test
    public void testParseJson(){
        String  docInfoJson = "{\n" +
                "    \"info\":\n" +
                "    {\n" +
                "        \"title\":\"testTitle\"\n" +
                "    },\n" +
                "    \"paths\":\n" +
                "    {\n" +
                "        \"testPath1\":\n" +
                "        {\n" +
                "            \"post\":\n" +
                "            {\n" +
                "                \"summary\":\"testSummary\",\n" +
                "                \"description\":\"testDescription\",\n" +
                "                \"produces\": [\"application/json\", \"application/xml\"],\n" +
                "                \"multiple\":\"true\",\n" +
                "                \"module_order\":1,\n" +
                "                \"api_order\":1\n" +
                "            }\n" +
                "        }\n" +
                "    }\n" +
                "}";
        JsonObject docRoot = GsonUtils.getInstance().fromJson(docInfoJson, JsonObject.class);
        docRoot.addProperty("basePath", "/" + "testClusterName");
        DocInfo docInfo = swaggerDocParser.parseJson(docRoot);
        assert docInfo.getDocModuleList().get(0).getModule().equals("testTitle");
    }
}
