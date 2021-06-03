package org.apache.shenyu.common.utils;

import org.apache.shenyu.common.constant.Constants;

import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Path template.
 */
public class PathTemplate {

    private final Pattern matchPattern;

    private final List<String> variableNames;

    public PathTemplate(final String template) {
        TemplateInfo info = PathTemplate.TemplateInfo.parse(template);
        this.variableNames = Collections.unmodifiableList(info.getVariableNames());
        this.matchPattern = info.getMatchPattern();
    }

    /**
     * match.
     *
     * @param uri the uri
     * @return map
     */
    public Map<String, String> match(final String uri) {
        Map<String, String> result = new LinkedHashMap<>(this.variableNames.size());
        Matcher matcher = this.matchPattern.matcher(uri);
        if (matcher.find()) {
            for (int i = 1; i <= matcher.groupCount(); i++) {
                String name = this.variableNames.get(i - 1);
                String value = matcher.group(i);
                result.put(name, value);
            }
        }
        return result;
    }

    /**
     * TemplateInfo.
     */
    private static class TemplateInfo {

        private final List<String> variableNames;

        private final Pattern pattern;

        private TemplateInfo(final List<String> vars, final Pattern pattern) {
            this.variableNames = vars;
            this.pattern = pattern;
        }

        public List<String> getVariableNames() {
            return this.variableNames;
        }

        public Pattern getMatchPattern() {
            return this.pattern;
        }

        public static PathTemplate.TemplateInfo parse(final String uriTemplate) {
            int level = 0;
            List<String> variableNames = new ArrayList<>();
            StringBuilder pattern = new StringBuilder();
            StringBuilder builder = new StringBuilder();
            for (int i = 0, len = uriTemplate.length(); i < len; i++) {
                char c = uriTemplate.charAt(i);
                if (c == Constants.LEFT) {
                    level++;
                    if (level == 1) {
                        // start
                        pattern.append(quote(builder));
                        builder = new StringBuilder();
                        continue;
                    }
                } else if (c == Constants.RIGHT) {
                    level--;
                    if (level == 0) {
                        // end
                        String variable = builder.toString();
                        pattern.append(Constants.PATTERN);
                        variableNames.add(variable);
                        builder = new StringBuilder();
                        continue;
                    }
                }
                builder.append(c);
            }
            if (builder.length() > 0) {
                pattern.append(quote(builder));
            }
            return new TemplateInfo(variableNames, Pattern.compile(pattern.toString()));
        }

        private static String quote(final StringBuilder builder) {
            return (builder.length() > 0 ? Pattern.quote(builder.toString()) : "");
        }
    }
}
