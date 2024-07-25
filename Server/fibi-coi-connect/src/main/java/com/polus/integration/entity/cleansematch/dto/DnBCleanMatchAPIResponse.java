package com.polus.integration.entity.cleansematch.dto;

import lombok.Data;

@Data
public class DnBCleanMatchAPIResponse {
    private String httpStatusCode;
    private String fullResponse;
    private String transactionID;
    private Integer candidatesMatchedQuantity;
    private String matchCandidates;
    private String highestMatch;
    private Integer highestMatchConfidenceCode;
    private String errorCode;
    private String errorMessage;
    private String errorDetails;
}
