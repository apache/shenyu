package org.dromara.soul.plugin.base.utils;

import java.net.URI;
import org.apache.commons.lang3.StringUtils;

/**
 * uri util.
 * @author liangziqiang
 */
public class UriUtils {


    /**
     * create URI {@link URI}.
     * @param uri uri string eg:/fallback
     * @return created {@link URI} from uri
     */
    public static URI createUri(final String uri) {
        if (StringUtils.isNotBlank(uri)) {
            return URI.create(uri);
        }
        return null;
    }

}
