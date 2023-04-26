package org.apache.shenyu.admin.service.manager.impl;
import org.apache.shenyu.admin.model.bean.DocInfo;
import org.apache.shenyu.admin.model.bean.DocItem;
import org.apache.shenyu.admin.service.manager.impl.DocManagerImpl;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;

import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.util.DigestUtils;

import java.lang.reflect.Field;

import java.util.*;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Consumer;

import static org.junit.jupiter.api.Assertions.assertEquals;


@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class DocManagerImplTest {
    @InjectMocks
    DocManagerImpl docManager;


    @Test
    public void testAddDocInfo(){
        String clusterName = "testClusterName";
        AtomicBoolean atomicBoolean = new AtomicBoolean(false);
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

        docManager.addDocInfo(clusterName, docInfoJson, docInfo -> atomicBoolean.set(true));

        assertEquals(true,atomicBoolean.get());

        Class<DocManagerImpl> DocManageClass = DocManagerImpl.class;
        Map<String, DocInfo> docDefinitionMap;
        try {
            Field field = DocManageClass.getDeclaredField("DOC_DEFINITION_MAP");
            field.setAccessible(true);
            docDefinitionMap = (Map<String, DocInfo>) field.get(null);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
        DocInfo docInfo = docDefinitionMap.get("testTitle");
        assertEquals(docInfo.getClusterName(),"testClusterName");
        assertEquals(docInfo.getTitle(),"testTitle");
        assertEquals(docManager.getByTitle("testTitle").getClusterName(),"testClusterName");
    }
    @Test
    public void testGetByTitle() {
        Class<DocManagerImpl> DocManageClass = DocManagerImpl.class;
        Map<String, DocInfo> docDefinitionMap;
        try {
            Field field = DocManageClass.getDeclaredField("DOC_DEFINITION_MAP");
            field.setAccessible(true);
            docDefinitionMap = (Map<String, DocInfo>) field.get(null);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
        DocManagerImpl docManager = new DocManagerImpl();
        String title = "testTitle";
        DocInfo expectedDocInfo = new DocInfo();
        expectedDocInfo.setTitle("testTitle");
        docDefinitionMap.put("testTitle",expectedDocInfo);
        assertEquals(expectedDocInfo, docManager.getByTitle(title));
    }

    @Test
    public void testGetDocItem() {
        Class<DocManagerImpl> DocManageClass = DocManagerImpl.class;
        Map<String, DocItem> itemDocMap;
        try {
            Field field = DocManageClass.getDeclaredField("ITEM_DOC_MAP");
            field.setAccessible(true);
            itemDocMap = (Map<String, DocItem>) field.get(null);
        }catch (Exception e){
            throw new RuntimeException(e);
        }
        String id = "1";
        DocItem expectedDocItem = new DocItem();
        expectedDocItem.setId(id);
        itemDocMap.put(id,expectedDocItem);
        assertEquals(expectedDocItem,docManager.getDocItem(id));
    }

    @Test
    public void testListAll() {
        Class<DocManagerImpl> DocManageClass = DocManagerImpl.class;
        Map<String, DocInfo> docDefinitionMap;
        try {
            Field field = DocManageClass.getDeclaredField("DOC_DEFINITION_MAP");
            field.setAccessible(true);
            docDefinitionMap = (Map<String, DocInfo>) field.get(null);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
        DocManagerImpl docManager = new DocManagerImpl();
        String title1 = "testTitle1";
        String title2 = "testTitle2";
        DocInfo expectedDocInfo1 = new DocInfo();
        DocInfo expectedDocInfo2 = new DocInfo();
        expectedDocInfo1.setTitle(title1);
        expectedDocInfo2.setTitle(title2);
        docDefinitionMap.put(title1,expectedDocInfo1);
        docDefinitionMap.put(title2,expectedDocInfo2);
        Collection<DocInfo> docInfos = docManager.listAll();
        assertEquals(true,!docInfos.isEmpty());
    }
    @Test
    public void testGetDocMd5(){
        Class<DocManagerImpl> DocManageClass = DocManagerImpl.class;
        Map<String, String> clusterMd5Map;
        try {
            Field field = DocManageClass.getDeclaredField("CLUSTER_MD5_MAP");
            field.setAccessible(true);
            clusterMd5Map = (Map<String, String>) field.get(null);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
        String title = "testTitle";
        clusterMd5Map.put(title, DigestUtils.md5DigestAsHex(title.getBytes()));
        assertEquals(DigestUtils.md5DigestAsHex(title.getBytes()),docManager.getDocMd5(title));
    }

    @Test
    public void testRemove(){
        Class<DocManagerImpl> DocManageClass = DocManagerImpl.class;
        Map<String, String> clusterMd5Map;
        try {
            Field field = DocManageClass.getDeclaredField("CLUSTER_MD5_MAP");
            field.setAccessible(true);
            clusterMd5Map = (Map<String, String>) field.get(null);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
        String title = "testTitle";
        clusterMd5Map.put(title, DigestUtils.md5DigestAsHex(title.getBytes()));
        assertEquals(DigestUtils.md5DigestAsHex(title.getBytes()),docManager.getDocMd5(title));
        docManager.remove(title);
        assertEquals(null,docManager.getDocMd5(title));
    }
}
