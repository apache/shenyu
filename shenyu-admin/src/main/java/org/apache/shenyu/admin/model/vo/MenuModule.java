package org.apache.shenyu.admin.model.vo;

import java.util.List;

/**
 * MenuModule.
 * MenuModule
 */
public class MenuModule {

    private String label;

    private List<MenuDocItem> children;

    /**
     * getLabel.
     *
     * @return String
     */
    public String getLabel() {
        return label;
    }

    /**
     * setLabel.
     *
     * @param label label
     */
    public void setLabel(final String label) {
        this.label = label;
    }

    /**
     * getChildren.
     *
     * @return List
     */
    public List<MenuDocItem> getChildren() {
        return children;
    }

    /**
     * setChildren.
     *
     * @param children children
     */
    public void setChildren(final List<MenuDocItem> children) {
        this.children = children;
    }
}
