package com.polus.fibicomp.util.claim;

import java.text.SimpleDateFormat;

/**
 * Custom Jxls Date Formatter
 *
 * @author ajin.vs
 * @since 13/09/2022
 */
@SuppressWarnings("unused")
public class JxlsDateFormatter {

    public SimpleDateFormat formatter(String pattern) {
        return new SimpleDateFormat(pattern);
    }
}
