package org.apache.shenyu.web.loader;

import org.junit.*;
import org.junit.runner.RunWith;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.*;

/**
 * The TestCase for ShenyuPluginLoader.
 */
@RunWith(PowerMockRunner.class)
@PrepareForTest(ShenyuPluginLoader.class)
public class ShenyuPluginLoaderTest {

    private  ShenyuPluginLoader shenyuPluginLoader;

    private  File[] files ;

    private Path path;

    @BeforeClass
    public static void beforeClass() {
    }

    @AfterClass
    public static void afterClass() {
    }

    @Before
    public void setUp() throws Exception {
        shenyuPluginLoader=ShenyuPluginLoader.getInstance();
        path = Files.createTempFile("plugin", ".zip");
        String pluginClas="public class ApacheDubboPlugin {\n" +
                "}";
        try (OutputStream os = Files.newOutputStream(path);
             ZipOutputStream zos = new ZipOutputStream(os)) {
            ZipEntry e = new ZipEntry("org.apache.shenyu.plugin.ApacheDubboPlugin.class");
            zos.putNextEntry(e);
            zos.write(pluginClas.getBytes());
        }
    }

    /**
     *  test for  getInstance.
     */
    @Test
    public void getInstance() {
        assertThat(ShenyuPluginLoader.getInstance()).isEqualTo(shenyuPluginLoader);
    }

    /**  test for  loadExtendPlugins with no plugin.
     * @throws Exception
     */
    @Test
    public void loadExtendPlugins_with_empty()  throws Exception {
        shenyuPluginLoader=PowerMockito.spy(shenyuPluginLoader);
        PowerMockito.doReturn(new File[0]).when(shenyuPluginLoader).listFiles(any());
        Assert.assertEquals( shenyuPluginLoader.loadExtendPlugins("").size(),  0);
    }

    /** test for  loadExtendPlugins with  plugins.
     * @throws Exception
     */
    @Test
    public void loadExtendPlugins_with_jar() throws Exception {
        shenyuPluginLoader= PowerMockito.spy(shenyuPluginLoader);
        PowerMockito.doReturn(null).when(shenyuPluginLoader).getPluginPath(anyString());
        files= new File[]{path.toFile()};
        PowerMockito.doReturn(files).when(shenyuPluginLoader).listFiles(null);
        PowerMockito.doReturn(new Object()).when( shenyuPluginLoader, "getOrCreateInstance",anyString());
        Assert.assertEquals(1,  shenyuPluginLoader.loadExtendPlugins("").size());
    }

}