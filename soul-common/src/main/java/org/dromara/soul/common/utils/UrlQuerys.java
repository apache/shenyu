package org.dromara.soul.common.utils;

import org.springframework.util.StringUtils;

import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class UrlQuerys {

    private static final Pattern PATTERN = Pattern.compile("([^&=]+)(=?)([^&]+)?");

    /**
     * getQuery.
     *
     * @param uri uri
     * @return String string
     */
    public static String getQuery(final String uri) {
        return GsonUtils.getInstance().toJson(initQueryParams(uri));
    }

    static Map<String, String> initQueryParams(final String query) {
        final Map<String, String> queryParams = new LinkedHashMap<>();
        if (query != null) {
            final Matcher matcher = PATTERN.matcher(query);
            while (matcher.find()) {
                String name = decodeQueryParam(matcher.group(1));
                String eq = matcher.group(2);
                String value = matcher.group(3);
                value = value != null ? decodeQueryParam(value) : (StringUtils.hasLength(eq) ? "" : null);
                queryParams.put(name, value);
            }
        }
        return queryParams;
    }

    static String decodeQueryParam(final String value) {
        try {
            return URLDecoder.decode(value, "UTF-8");
        } catch (UnsupportedEncodingException exception) {
            return URLDecoder.decode(value);
        }
    }
}
