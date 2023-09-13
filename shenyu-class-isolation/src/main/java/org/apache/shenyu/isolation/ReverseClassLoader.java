package org.apache.shenyu.isolation;

import java.net.URL;
import java.net.URLClassLoader;


public class ReverseClassLoader extends URLClassLoader {

    public ReverseClassLoader(URL[] urls, ClassLoader parent) {
        super(urls, parent);
    }

    public ReverseClassLoader(URL[] urls) {
        super(urls);
    }

    public void addURL(URL url) {
        super.addURL(url);
    }

    @Override
    public Class<?> loadClass(String name) throws ClassNotFoundException {
        return loadClass(name, false);
    }

    @Override
    protected Class<?> loadClass(String name, boolean resolve) throws ClassNotFoundException {
        synchronized (getClassLoadingLock(name)) {
            Class<?> c = null;
            if (c == null) {
                c = findLoadedClass(name);
                try {
                    if (c == null) {
                        c=findClass(name);
                    }
                } catch (ClassNotFoundException e) {
                }
            }
            if (c == null) {
                c = super.loadClass(name, resolve);
            }
            if (resolve) {
                resolveClass(c);
            }
            return c;
        }
    }
}




























