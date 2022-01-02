package org.apache.shenyu.agent.plugin.tracing.common.constant;

/**
 * The type Tracing constants.
 */
public final class TracingConstants {

    /**
     * The constant NAME.
     */
    public static final String NAME = "shenyu";

    /**
     * The constant ROOT_SPAN.
     */
    public static final String ROOT_SPAN = "/shenyu/root";

    public static final String COMPONENT = "component";

    public static final String HTTP_URL = "http.url";

    public static final String HTTP_STATUS = "http.status_code";

    public static final String SHENYU_AGENT = "shenyu-agent";

    /**
     * The type Error log tags.
     */
    public static final class ErrorLogTags {

        /**
         * The constant EVENT.
         */
        public static final String EVENT = "event";

        /**
         * The constant EVENT_ERROR_TYPE.
         */
        public static final String EVENT_ERROR_TYPE = "error";

        /**
         * The constant ERROR_KIND.
         */
        public static final String ERROR_KIND = "error.kind";

        /**
         * The constant MESSAGE.
         */
        public static final String MESSAGE = "message";
    }
}
