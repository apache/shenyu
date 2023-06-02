package org.apache.shenyu.admin.discovery.parse;

import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.common.dto.ProxySelectorData;

import java.util.List;

/**
 * parse value to List<ProxySelectorData>
 */
public interface keyValueParser {

    /**
     * parseValue.
     *
     * @param value value
     * @return List<DiscoveryUpstreamData>
     */
    List<DiscoveryUpstreamData> parseValue(String value);

    /**
     * parseKey.
     *
     * @param key discovery key
     * @return ProxySelectorData.
     */
    ProxySelectorData parseKey(String key);

}
