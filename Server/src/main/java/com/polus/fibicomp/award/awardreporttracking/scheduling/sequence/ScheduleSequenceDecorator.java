package com.polus.fibicomp.award.awardreporttracking.scheduling.sequence;


/**
 * This class implements decorator pattern for SheduleSequence.
 */
public abstract class ScheduleSequenceDecorator  implements ScheduleSequence {
    
    protected ScheduleSequence scheduleSequence; 
    
    public ScheduleSequenceDecorator(ScheduleSequence scheduleSequence) {
        this.scheduleSequence = scheduleSequence;
    }

}
