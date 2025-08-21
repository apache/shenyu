package org.apache.shenyu.admin.model.vo;

import java.util.List;
import java.util.Map;

public class InstanceDataVisualVO {
    Map<Integer, Long> pieData;
    List<InstanceDataVisualLineVO> lineList;

    public Map<Integer, Long> getPieData() {
        return pieData;
    }

    public void setPieData(Map<Integer, Long> pieData) {
        this.pieData = pieData;
    }

    public List<InstanceDataVisualLineVO> getLineList() {
        return lineList;
    }

    public void setLineList(List<InstanceDataVisualLineVO> lineList) {
        this.lineList = lineList;
    }
}
