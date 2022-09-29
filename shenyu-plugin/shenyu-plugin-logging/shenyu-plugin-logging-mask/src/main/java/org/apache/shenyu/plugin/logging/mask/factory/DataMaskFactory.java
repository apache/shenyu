package org.apache.shenyu.plugin.logging.mask.factory;

import org.apache.shenyu.plugin.logging.mask.spi.ShenyuDataMask;
import org.apache.shenyu.spi.ExtensionLoader;

/**
 * shenyu logging mask factory.
 */
public final class DataMaskFactory {

    /**
     * shenyu logging mask algorithm selector.
     *
     * @param source source data
     * @param algorithm algorithm
     * @return masked data
     */
    public static String selectMask(final String source, final String algorithm) {
        ShenyuDataMask dataMask = ExtensionLoader.getExtensionLoader(ShenyuDataMask.class).getJoin(algorithm);
        return dataMask.mask(source);
    }
}
