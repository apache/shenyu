package org.apache.shenyu.admin;

import org.apache.shenyu.admin.mapper.NamespacePluginRelMapper;
import org.apache.shenyu.admin.model.vo.NamespacePluginVO;
import org.junit.jupiter.api.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;

import java.util.Arrays;
import java.util.List;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;

@RunWith(SpringRunner.class)
@SpringBootTest
public class NamespacePluginRelMapperTest {

    @Autowired
    private NamespacePluginRelMapper namespacePluginRelMapper;

    @Test
    public void testSelectByIds() {
        List<String> ids = Arrays.asList("1801816010882822151","1801816010882822155");
        List<NamespacePluginVO> result = namespacePluginRelMapper.selectByIds(ids);
        assertNotNull(result);
        assertFalse(result.isEmpty());
    }
}
