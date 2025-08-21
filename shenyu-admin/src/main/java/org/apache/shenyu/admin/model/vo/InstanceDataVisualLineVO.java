package org.apache.shenyu.admin.model.vo;

import java.util.List;

public class InstanceDataVisualLineVO {

    public InstanceDataVisualLineVO() {
    }

    public InstanceDataVisualLineVO(String name, List<Long> date) {
        this.name = name;
        this.date = date;
    }

    private String name;
    private List<Long> date;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public List<Long> getDate() {
        return date;
    }

    public void setDate(List<Long> date) {
        this.date = date;
    }
}
