package com.polus.fibicomp.util.claim;

import org.apache.commons.beanutils.PropertyUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.math.BigDecimal;
import java.util.Collection;
import java.util.Iterator;

/**
 * Custom Jxls aggregator
 *
 * @author ajin.vs
 * @since 25/08/2022
 */
@SuppressWarnings("unused")
public class JxlsAggregator {

    protected static Logger logger = LogManager.getLogger(JxlsAggregator.class.getName());

    public BigDecimal sum(Collection<Object> beans, String property) {
        BigDecimal result = BigDecimal.ZERO;
        try {
            for (Iterator<Object> iterator = beans.iterator(); iterator.hasNext(); ) {
                Object obj = iterator.next();
                BigDecimal value = (BigDecimal) PropertyUtils.getProperty(obj, property);
                result = result.add(value);
            }
        } catch (Exception e) {
            logger.error("JxlsAggregator Sum : {}", e.getMessage());
        }
        return result;
    }
}
