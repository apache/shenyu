package org.apache.shenyu.plugin.logging.mask.spi;

import org.apache.shenyu.common.utils.Md5Utils;
import org.apache.shenyu.spi.Join;

/**
 * md5 encrypt data mask.
 */
@Join
public class Md5EncryptDataMask extends AbstractShenyuDataMask {
    @Override
    protected String doMask(String source) {
        return Md5Utils.md5(source);
    }
}
