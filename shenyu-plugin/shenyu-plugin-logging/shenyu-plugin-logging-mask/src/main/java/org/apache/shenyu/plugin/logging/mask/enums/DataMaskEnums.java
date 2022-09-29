package org.apache.shenyu.plugin.logging.mask.enums;

/**
 * data mask enums.
 */
public enum DataMaskEnums {

    CHARACTER_REPLACE("dataMaskByCharReplace"),

    MD5_ENCRYPT("dataMaskByMD5"),
    ;
    private final String dataMaskAlg;

    DataMaskEnums(String dataMaskAlg) {
        this.dataMaskAlg = dataMaskAlg;
    }

    public String getDataMaskAlg() {
        return dataMaskAlg;
    }
}
