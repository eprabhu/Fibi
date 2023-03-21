package com.polus.fibicomp.util.claim;

import com.polus.fibicomp.claims.pojo.ClaimSummaryDetails;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.stream.Collectors;
import java.util.HashMap;
import java.util.Collection;
import java.util.Arrays;

/**
 * Custom Jxls Comparator
 *
 * @author ajin.vs
 * @version 1.2
 * @since 22/07/2022
 */
@SuppressWarnings("unused")
public class JxlsComparator {

    protected static Logger logger = LogManager.getLogger(JxlsComparator.class.getName());

    public List<LinkedHashMap<Object, Object>> filterByCurrentClaimSummDetails(List<LinkedHashMap<Object, Object>> currentClaimSummDetails,
                                                                               List<LinkedHashMap<Object, Object>> claimSummaryDetails, String categoryCode) {
        List<LinkedHashMap<Object, Object>> filteredSummaryDetails = new ArrayList<>();
        List<LinkedHashMap<Object, Object>> filteredCurrentClaimSummDetails = new ArrayList<>();
        try {
            filteredCurrentClaimSummDetails = currentClaimSummDetails.stream()
                    .filter(summDetail -> summDetail.get("internalOrderCode").toString().substring(15, 18).equals(categoryCode)).collect(Collectors.toList());
            List<LinkedHashMap<Object, Object>> filteredClaimSummaryDetails = claimSummaryDetails.stream()
                    .filter(summDetail -> summDetail.get("internalOrderCode").toString().substring(15, 18).equals(categoryCode)).collect(Collectors.toList());
            filteredCurrentClaimSummDetails.forEach(curSummaryDetail -> {
                filteredClaimSummaryDetails.forEach(summaryDetail -> {
                    String currentIntOrderCode = (String) curSummaryDetail.get("internalOrderCode");
                    String internalOrderCode = (String) summaryDetail.get("internalOrderCode");
                    if (currentIntOrderCode.substring(15, 20).equals(internalOrderCode.substring(15, 20))) {
                        filteredSummaryDetails.add(summaryDetail);
                    }

                });
            });

            if (filteredCurrentClaimSummDetails.size() > filteredClaimSummaryDetails.size()) {
                for (int i = 0; i < (filteredCurrentClaimSummDetails.size() - filteredClaimSummaryDetails.size()); i++) {
                    LinkedHashMap<Object, Object> dummyObj = new LinkedHashMap<>();
                    dummyObj.put("internalOrderCode", categoryCode);
                    filteredSummaryDetails.add(dummyObj);
                }
            }

        } catch (Exception e) {
            logger.error("JxlsComparator : filterByCurrentClaimSumDetails : {}", e.getMessage());
            filteredSummaryDetails.clear();
            for (int i = 0; i < (filteredCurrentClaimSummDetails.size()); i++) {
                LinkedHashMap<Object, Object> dummyObj = new LinkedHashMap<>();
                dummyObj.put("internalOrderCode", categoryCode);
                filteredSummaryDetails.add(dummyObj);
            }
        } finally {
            if (filteredSummaryDetails.size() == 0) {
                LinkedHashMap<Object, Object> dummyObj = new LinkedHashMap<>();
                dummyObj.put("internalOrderCode", categoryCode);
                filteredSummaryDetails.add(dummyObj);
            }
        }
        return filteredSummaryDetails;
    }

    public List<LinkedHashMap<Object, Object>> filterClaimSummDetailsByCatCode(List<LinkedHashMap<Object, Object>> claimSummDetails, String categoryCode) {
        List<LinkedHashMap<Object, Object>> filteredClaimSummDetails = new ArrayList<>();
        try {
            filteredClaimSummDetails = claimSummDetails.stream()
                    .filter(summDetail -> summDetail.get("internalOrderCode").toString().substring(15, 18).equals(categoryCode)).collect(Collectors.toList());
        } catch (Exception e) {
            logger.error("JxlsComparator : filterClaimSummDetailsByCatCode : {}", e.getMessage());
        } finally {
            if (filteredClaimSummDetails != null && filteredClaimSummDetails.isEmpty()) {
                LinkedHashMap<Object, Object> dummyObj = new LinkedHashMap<>();
                dummyObj.put("internalOrderCode", categoryCode);
                filteredClaimSummDetails.add(dummyObj);
            }
            if (filteredClaimSummDetails == null) {
                filteredClaimSummDetails = new ArrayList<>();
                LinkedHashMap<Object, Object> dummyObj = new LinkedHashMap<>();
                dummyObj.put("internalOrderCode", categoryCode);
                filteredClaimSummDetails.add(dummyObj);
            }
        }
        return filteredClaimSummDetails;
    }

    public List<Object> groupTransactions(List<LinkedHashMap<Object, Object>> claimSummDetails, String categoryCode) {
        List<Object> transactions = new ArrayList<>();
        try {
            List<LinkedHashMap<Object, Object>> filteredClaimSummDetails = claimSummDetails.stream()
                    .filter(summDetail -> summDetail.get("internalOrderCode").toString().substring(15, 18).equals(categoryCode)).collect(Collectors.toList());
            filteredClaimSummDetails.forEach(obj -> {
                List<HashMap<Object, Object>> groupedTransactions = (List<HashMap<Object, Object>>) obj.get("groupedTransactions");
                groupedTransactions.forEach(transactionObj -> transactions.addAll((Collection<?>) transactionObj.get("transactions")));
            });
        } catch (Exception e) {
            logger.error("JxlsComparator : groupTransactions : {}", e.getMessage());
        }
        return transactions;
    }

    public List<Object> conditionedCSDFilter(List<ClaimSummaryDetails> objects, String fieldName, String values) {
        List<Object> result = new ArrayList<>();
        try {
            objects.forEach(obj -> {
                String[] fieldValues = values.split(",");
                try {
                    Field field = Class.forName("com.polus.fibicomp.claims.pojo.ClaimSummaryDetails").getDeclaredField(fieldName);
                    field.setAccessible(true);
                    if (Arrays.stream(fieldValues).anyMatch(field.get(obj)::equals)) {
                        result.add(obj);
                    }
                } catch (Exception e) {
                    logger.error("JxlsAggregator conditionedCSDFilter : {}", e.getMessage());
                }
            });
        } catch (Exception e) {
            logger.error("JxlsAggregator conditionedCSDFilter : {}", e.getMessage());
        }
        return result;
    }
}
