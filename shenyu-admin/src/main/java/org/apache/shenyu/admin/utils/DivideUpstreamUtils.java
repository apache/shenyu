package org.apache.shenyu.admin.utils;

import org.apache.shenyu.common.dto.convert.DivideUpstream;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;

import java.util.Optional;

/**
 * Build upstream.
 */
public class DivideUpstreamUtils {

    /**
     * build DivideUpstream.
     *
     * @param metaDataRegisterDTO metaDataRegisterDTO
     * @return divideUpstream divideUpstream
     */
    public static DivideUpstream buildDivideUpstream(final MetaDataRegisterDTO metaDataRegisterDTO) {
        return DivideUpstream.builder().upstreamHost("localhost").protocol("http://").upstreamUrl(buildUrl(metaDataRegisterDTO)).weight(50).build();
    }

    /**
     * build Url.
     *
     * @param metaDataRegisterDTO metaDataRegisterDTO
     * @return String String
     */
    public static String buildUrl(final MetaDataRegisterDTO metaDataRegisterDTO) {
        return Optional.ofNullable(String.join(":", metaDataRegisterDTO.getHost(), String.valueOf(metaDataRegisterDTO.getPort()))).orElse(null);
    }
}
