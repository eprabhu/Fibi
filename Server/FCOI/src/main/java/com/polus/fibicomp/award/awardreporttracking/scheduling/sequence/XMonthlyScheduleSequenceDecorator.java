package com.polus.fibicomp.award.awardreporttracking.scheduling.sequence;

import org.quartz.TriggerUtils;
import org.quartz.impl.triggers.CronTriggerImpl;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.TimeZone;

/**
 * This class is variation of DefaultSchduleSequence, it adds logic wrapper to dates returned by Quartz util.
 * <p>
 * Logic wrapper filters dates, to generate schedule for every month, every other month, etc ...
 * <p>
 * This implementation is very specifically addressed to deal with monthly, quarterly, annually, etc date filtering requirements. Implementation
 * uses org.quartz.TriggerUtils class to generate List of dates from Cron expression.
 * <p>
 * It uses current time zone, where application is hosted.
 * <p>
 * Note: Dates used in generating schedule must be wrapped with required time accuracy.
 * e.g.  Start Date: 02/01/09 10:10 End Date: 02/05/09 10:10 
 * Generated Dates will be in between 02/01/09 10:10 and 02/05/09 10:10.
 * Any date expected before 02/01/09 10:09 will be ignored. Date 02/01/09 10:00 will be ignored.
 * Any date expected after  02/05/09 10:11 will be ignored. Date 02/05/09 12:00 will be ignored.
 */
public class XMonthlyScheduleSequenceDecorator extends ScheduleSequenceDecorator {

    private Integer frequency;
    
    /**
     * Constructs a WeekScheduleSequence.java.
     * @param frequency can be weekly, biweekly, etc...
     */
    public XMonthlyScheduleSequenceDecorator(ScheduleSequence scheduleSequence, Integer frequency) {
        super(scheduleSequence);
        this.frequency = frequency;        
    }

    @Override
    @SuppressWarnings("unchecked")
    public List<Date> executeScheduleSequence(String expression, Date startDate, Date endDate) throws ParseException {

        CronTriggerImpl ct = new CronTriggerImpl(NAME, GROUP, JOBNAME, JOBGROUP, new Date(), null, expression);
        ct.setTimeZone(TimeZone.getDefault());
        List<Date> dates = TriggerUtils.computeFireTimesBetween(ct, null, startDate, endDate);

        if (frequency != 1) {
            
            List<Date> filteredDates = new ArrayList<Date>();
            
            int i=0;
            for(Date date: dates){
                if(i%frequency==0){
                    filteredDates.add(date);
                }
                i++;
            }            
            dates = filteredDates;
        }
        if (dates.isEmpty()) {
        	dates.add(startDate);
        }
        return dates;
    }
}
