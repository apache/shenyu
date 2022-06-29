package org.apache.shenyu.admin.model.query;

import com.fasterxml.jackson.annotation.JsonFormat;
import org.apache.shenyu.admin.model.page.condition.BaseExcludedSearchCondition;
import org.apache.shenyu.admin.model.page.condition.SearchCondition;
import org.apache.shenyu.common.utils.DateUtils;

import javax.validation.constraints.NotNull;
import java.util.Date;

/**
 * RecordLogQueryCondition.
 */
public class RecordLogQueryCondition extends BaseExcludedSearchCondition implements SearchCondition {
    
    /**
     * search keyword: log context.
     */
    private String keyword;
    
    
    /**
     * log type.
     */
    private String type;
    
    /**
     * start time.
     */
    @NotNull
    @JsonFormat(pattern = DateUtils.DATE_FORMAT_DATETIME)
    private Date startTime;
    
    /**
     * end time.
     */
    @NotNull
    @JsonFormat(pattern = DateUtils.DATE_FORMAT_DATETIME)
    private Date endTime;
    
    @Override
    public void setKeyword(final String keyword) {
        this.keyword = keyword;
    }
    
    @Override
    public String getKeyword() {
        return keyword;
    }
    
    
    /**
     * get startTime.
     *
     * @return time
     */
    public Date getStartTime() {
        return startTime;
    }
    
    /**
     * set startTime.
     *
     * @param startTime startTime
     */
    public void setStartTime(final Date startTime) {
        this.startTime = startTime;
    }
    
    /**
     * get endTime.
     *
     * @return time
     */
    public Date getEndTime() {
        return endTime;
    }
    
    /**
     * set endTime.
     *
     * @param endTime endTime
     */
    public void setEndTime(final Date endTime) {
        this.endTime = endTime;
    }
    
    /**
     * get type.
     *
     * @return type
     */
    public String getType() {
        return type;
    }
    
    /**
     * set type.
     *
     * @param type type
     */
    public void setType(final String type) {
        this.type = type;
    }
}
