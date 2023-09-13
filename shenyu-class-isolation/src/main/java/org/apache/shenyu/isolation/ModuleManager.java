package org.apache.shenyu.isolation;



import java.io.File;
import java.io.FilenameFilter;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.ServiceLoader;

public class ModuleManager {

    public static URLClassLoader initClassLoader(File dir) throws MalformedURLException {
        File[] jars = dir.listFiles(new FilenameFilter() {
            @Override
            public boolean accept(File dir, String name) {
                return name.endsWith(".jar");
            }
        });

        if (jars.length == 0) {
            return null;
        }

        URL[] classPath = new URL[jars.length + 1];
        classPath[0] = dir.toURI().toURL();

        for (int i = 1; i < classPath.length; i++) {
            classPath[i] = jars[i - 1].toURI().toURL();
        }

        return new ReverseClassLoader(classPath, ModuleManager.class.getClassLoader());
    }
}
