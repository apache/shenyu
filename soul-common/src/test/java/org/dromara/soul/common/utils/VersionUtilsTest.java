package org.dromara.soul.common.utils;

import org.junit.Test;

import static org.junit.Assert.assertNotNull;

/**
 * Test cases for VersionUtils
 *
 * @author linkuan
 */
public class VersionUtilsTest {

    /**
     * cover version from defaultVersion
     */
    @Test
    public void testFromDefaultVersion(){
        String version = VersionUtils.getVersion();
        System.out.println(version);
        assertNotNull(version);
    }

    /**
     * cover version from defaultVersion
     */
    @Test
    public void testFromDefault(){
        String version = VersionUtils.getVersion(Version.class, "2.0.0");
        System.out.println(version);
        assertNotNull(version);
    }

    /**
     * cover find version info from MANIFEST.MF
     */
    @Test
    public void testFromImplementationVersion()throws ClassNotFoundException{
        String version = VersionUtils.getVersion(Class.forName("java.lang.String"), "2.0.2");
        System.out.println(version);
        assertNotNull(version);
    }

    /**
     * cover version info from codeSource
     */
    @Test
    public void testFromCodeSource()throws ClassNotFoundException{
        String version = VersionUtils.getVersion(Class.forName("ch.qos.logback.classic.db.DBAppender"), "2.0.2");
        System.out.println(version);
        assertNotNull(version);
    }

    static class Version{


    }


}
