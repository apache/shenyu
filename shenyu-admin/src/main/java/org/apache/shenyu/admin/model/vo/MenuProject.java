package org.apache.shenyu.admin.model.vo;

import java.util.List;

/**
 * MenuProject.
 *
 * @author tanghc
 */
public class MenuProject {

    private String label;

    private List<MenuModule> children;

    /**
     * setLabel.
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
    public List<MenuModule> getChildren() {
        return children;
    }

    /**
     * setChildren.
     *
     * @param children children
     */
    public void setChildren(final List<MenuModule> children) {
        this.children = children;
    }
}
