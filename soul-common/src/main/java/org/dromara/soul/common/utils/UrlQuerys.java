package org.dromara.soul.common.utils;

import org.springframework.util.MultiValueMap;
import org.springframework.util.StringUtils;

import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.function.Supplier;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class UrlQuerys {

    private static final Pattern PATTERN = Pattern.compile("([^&=]+)(=?)([^&]+)?");

    /**
     * of.
     *
     * @param supplier supplier
     * @return String
     */
    public static String of(final Supplier<String> supplier) {
        return GsonUtils.getInstance().toJson(initQueryParams(supplier.get()));
    }

    /**
     * map.
     *
     * @param supplier supplier
     * @return String
     */
    public static String map(final Supplier<MultiValueMap> supplier) {
        return GsonUtils.getInstance().toJson(supplier.get().toSingleValueMap());
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
