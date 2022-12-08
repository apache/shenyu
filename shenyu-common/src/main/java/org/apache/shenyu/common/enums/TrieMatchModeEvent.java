package org.apache.shenyu.common.enums;


/**
 * Shenyu match mode event.
 */
public enum TrieMatchModeEvent {
    /**
     * ant path match.
     */
    ANT_PATH_MATCH("antPathMatch"),

    /**
     * path pattern.
     */
    PATH_PATTERN("pathPattern");

    private final String matchMode;

    TrieMatchModeEvent(final String matchMode) {
        this.matchMode = matchMode;
    }

    /**
     * get trie match mode.
     *
     * @return match mode
     */
    public String getMatchMode() {
        return matchMode;
    }
}
