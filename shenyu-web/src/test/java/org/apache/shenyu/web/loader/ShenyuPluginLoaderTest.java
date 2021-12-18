package org.apache.shenyu.web.loader;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.List;
import java.util.jar.JarOutputStream;
import java.util.zip.ZipEntry;

import static org.hamcrest.CoreMatchers.is;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.spy;

/**
 * Test for  ShenyuPluginLoader.
 */

public class ShenyuPluginLoaderTest {
    @Rule
    public TemporaryFolder folder = new TemporaryFolder();

    private ShenyuPluginLoader shenyuPluginLoader;

    private String path;

    @Before
    public void setUp() throws IOException {
        shenyuPluginLoader = ShenyuPluginLoader.getInstance();
        File jar = folder.newFile("plugin.jar");
        path = jar.getParent();
        FileOutputStream fos = new FileOutputStream(jar);
        BufferedOutputStream bos = new BufferedOutputStream(fos);
        try (JarOutputStream jos = new JarOutputStream(bos)) {
            String pluginClz = "public class DividePlugin {}";
            jos.putNextEntry(new ZipEntry("org.apache.shenyu.plugin.DividePlugin.class"));
            jos.write(pluginClz.getBytes());
            jos.closeEntry();
        }
    }

    @Test
    public void testGetInstance() {
        Assert.assertThat(shenyuPluginLoader, is(ShenyuPluginLoader.getInstance()));
    }

    @Test
    public void testGetPluginPathWithNoJar() throws IOException, ClassNotFoundException, InstantiationException, IllegalAccessException {
        List<ShenyuLoaderResult> pluginList = shenyuPluginLoader.loadExtendPlugins("test");
        Assert.assertThat(pluginList.size(), is(0));
    }

    @Test
    public void testGetPluginPathWithJar() throws IOException, ClassNotFoundException, InstantiationException, IllegalAccessException {
        ShenyuPluginLoader loader = spy(shenyuPluginLoader);

        doReturn(new Object()).when(loader).getOrCreateInstance(anyString());

        List<ShenyuLoaderResult> pluginList = loader.loadExtendPlugins(path);
        Assert.assertThat(pluginList.size(), is(1));
    }
}
