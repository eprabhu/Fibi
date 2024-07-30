package com.polus.common.utils;

import org.springframework.stereotype.Component;

import java.sql.Timestamp;
import java.util.Calendar;
import java.util.Date;

@Component
public class CommonUtils {

    public Date getCurrentDate() {
        Calendar c = Calendar.getInstance();
        c.setTime(new Date());
        return c.getTime();
    }

    public Timestamp getCurrentTimestamp() {
        return new Timestamp(this.getCurrentDate().getTime());
    }

}
