package org.apache.shenyu.admin.model.page;

import javax.validation.Valid;
import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;

/**
 * page condition.
 *
 * @author likeugo
 */
public class PageCondition<T> {
    
    /**
     * current page num.
     */
    @NotNull
    private Integer pageNum;
    
    /**
     * page size.
     */
    @NotNull
    @Max(value = 1000, message = "size max support is 1000")
    @Min(value = 1, message = "size min support is 1")
    private Integer pageSize;
    
    /**
     * 查询条件
     */
    @Valid
    @NotNull
    private T condition;
    
    public Integer getPageNum() {
        return pageNum;
    }
    
    public void setPageNum(final Integer pageNum) {
        this.pageNum = pageNum;
    }
    
    public Integer getPageSize() {
        return pageSize;
    }
    
    public void setPageSize(final Integer pageSize) {
        this.pageSize = pageSize;
    }
    
    public T getCondition() {
        return condition;
    }
    
    public void setCondition(final T condition) {
        this.condition = condition;
    }
}
