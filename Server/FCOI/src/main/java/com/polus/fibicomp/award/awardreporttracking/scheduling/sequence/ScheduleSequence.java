package com.polus.fibicomp.award.awardreporttracking.scheduling.sequence;

import java.text.ParseException;
import java.util.Date;
import java.util.List;

public interface ScheduleSequence {
        
    public static final String NAME = "t";
    
    public static final String GROUP = "g";
    
    public static final String JOBNAME = "j";
    
    public static final String JOBGROUP = "g";
    
    /**
     * This method expects to generate list of dates between start and end date using cron expression.
     * 
     * @param expression can be any valid CronExpression.
     * @param startDate, is expression's begin date.
     * @param endDate, is expression's end date.
     * @return list of dates is returned.
     * @throws ParseException can thrown in case of invalid expression.
     */
    public List<Date> executeScheduleSequence(String expression, Date startDate, Date endDate) throws ParseException;

}
