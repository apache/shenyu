package org.apache.shenyu.admin.model.page;

import javax.validation.Valid;
import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;

/**
 * page condition.
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
    
    /**
     * get page num.
     *
     * @return page num
     */
    public Integer getPageNum() {
        return pageNum;
    }
    
    /**
     * set page num.
     *
     * @param pageNum page num
     */
    public void setPageNum(final Integer pageNum) {
        this.pageNum = pageNum;
    }
    
    /**
     * get page size.
     *
     * @return page size
     */
    public Integer getPageSize() {
        return pageSize;
    }
    
    /**
     * page size.
     *
     * @param pageSize page size
     */
    public void setPageSize(final Integer pageSize) {
        this.pageSize = pageSize;
    }
    
    /**
     * get condition.
     *
     * @return condition
     */
    public T getCondition() {
        return condition;
    }
    
    /**
     * set condition.
     *
     * @param condition condition
     */
    public void setCondition(final T condition) {
        this.condition = condition;
    }
}
