package org.apache.shenyu.admin.model.result;

import java.io.Serializable;

/**
 * ConfigImportResult.
 */
public class ConfigImportResult implements Serializable {

    private static final long serialVersionUID = 7527987507527292299L;

    private Integer successCount;

    private String failMessage;

    /**
     * Instantiates a new config import result.
     */
    public ConfigImportResult() {
    }

    /**
     * Instantiates a new config import result.
     *
     * @param successCount success count
     * @param failMessage  fail message
     */
    public ConfigImportResult(Integer successCount, String failMessage) {
        this.successCount = successCount;
        this.failMessage = failMessage;
    }

    /**
     * return success.
     *
     * @return {@linkplain ConfigImportResult}
     */
    public static ConfigImportResult success() {
        return success(0);
    }

    /**
     * return success.
     *
     * @return {@linkplain ConfigImportResult}
     */
    public static ConfigImportResult success(Integer successCount) {
        return new ConfigImportResult(successCount, "");
    }

    /**
     * return success.
     *
     * @return {@linkplain ConfigImportResult}
     */
    public static ConfigImportResult fail(Integer successCount, String failMessage) {
        return new ConfigImportResult(successCount, failMessage);
    }

    /**
     * Gets the success count.
     *
     * @return the success count
     */
    public Integer getSuccessCount() {
        return successCount;
    }

    /**
     * Sets the success count.
     *
     * @param successCount the success count
     */
    public void setSuccessCount(Integer successCount) {
        this.successCount = successCount;
    }

    /**
     * Gets the fail message.
     *
     * @return the fail message
     */
    public String getFailMessage() {
        return failMessage;
    }

    /**
     * Sets the fail message.
     *
     * @param failMessage the fail message
     */
    public void setFailMessage(String failMessage) {
        this.failMessage = failMessage;
    }
}
