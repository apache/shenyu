package org.apache.shenyu.registry.api.event;

@FunctionalInterface
public interface ChangedEventListener {

    /**
     * Data changed event.
     */
    enum Event {

        /**
         * Added event.
         */
        ADDED,
        /**
         * Updated event.
         */
        UPDATED,
        /**
         * Deleted event.
         */
        DELETED,
        /**
         * Ignored event.
         */
        IGNORED
    }

    /**
     * On event.
     *
     * @param event the event
     */
    void onEvent(String key, String value, Event event);
}
