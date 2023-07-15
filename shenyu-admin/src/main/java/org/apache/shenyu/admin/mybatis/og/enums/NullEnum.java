package org.apache.shenyu.admin.mybatis.og.enums;

public enum NullEnum {
    NULL("", "NULL"),
    ;

    String value;

    String label;

    NullEnum(String value, String label) {
        this.value = value;
        this.label = label;
    }

    public String getValue() {
        return value;
    }

    public String getLabel() {
        return label;
    }

    @Override
    public String toString() {
        return label;
    }
}
