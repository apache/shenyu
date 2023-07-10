/**
 * Code reduced and simplified from zmq integration in Java. See
 * https://github.com/zeromq/jzmq/blob/3384ea1c04876426215fe76b5d1aabc58c099ca0/jzmq-jni/src/main/java/org/zeromq/EmbeddedLibraryTools.java.
 */

package org.apache.shenyu.wasm;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URL;

public class Native {
    public static final boolean LOADED_EMBEDDED_LIBRARY;

    static {
        LOADED_EMBEDDED_LIBRARY = loadEmbeddedLibrary();
    }

    private Native() {}

    public static String getCurrentPlatformIdentifier() {
        String osName = System.getProperty("os.name").toLowerCase();

        if (osName.contains("windows")) {
            osName = "windows";
        } else if (osName.contains("mac os x")) {
            osName = "darwin";
        } else {
            osName = osName.replaceAll("\\s+", "_");
        }

        return osName + "-" + System.getProperty("os.arch");
    }

    private static boolean loadEmbeddedLibrary() {
        boolean usingEmbedded = false;

        // attempt to locate embedded native library within JAR at following location:
        // /NATIVE/${os.arch}/${os.name}/[libwasmer.so|libwasmer.dylib|wasmer.dll]
        String[] libs;
        final String libsFromProps = System.getProperty("wasmer-native");

        if (libsFromProps == null) {
            libs = new String[]{"libwasmer_jni.so", "libwasmer_jni.dylib", "wasmer_jni.dll"};
        } else {
            libs = libsFromProps.split(",");
        }

        StringBuilder url = new StringBuilder();
        url.append("/org/wasmer/native/");
        url.append(getCurrentPlatformIdentifier()).append("/");

        URL nativeLibraryUrl = null;

        // loop through extensions, stopping after finding first one
        for (String lib: libs) {
            nativeLibraryUrl = Module.class.getResource(url.toString() + lib);

            if (nativeLibraryUrl != null) {
                break;
            }
        }

        if (nativeLibraryUrl != null) {
            // native library found within JAR, extract and load
            try {
                final File libfile = File.createTempFile("wasmer_jni", ".lib");
                libfile.deleteOnExit(); // just in case

                final InputStream in = nativeLibraryUrl.openStream();
                final OutputStream out = new BufferedOutputStream(new FileOutputStream(libfile));

                int len = 0;
                byte[] buffer = new byte[8192];

                while ((len = in.read(buffer)) > -1) {
                    out.write(buffer, 0, len);
                }

                out.close();
                in.close();
                System.load(libfile.getAbsolutePath());

                usingEmbedded = true;
            } catch (IOException x) {
                // mission failed, do nothing
            }

        }

        return usingEmbedded;
    }
}
