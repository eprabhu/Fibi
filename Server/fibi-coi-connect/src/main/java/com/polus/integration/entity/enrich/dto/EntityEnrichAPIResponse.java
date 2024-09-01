package com.polus.integration.entity.enrich.dto;

import java.util.List;

import com.polus.integration.entity.enrich.dto.DnBEnrichAPIResponse.Organization;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class EntityEnrichAPIResponse {
	private String httpStatusCode;	
	private String transactionID;	
	private Organization organization;
	private String errorCode;
	private String errorMessage;
	private String errorDetails;
}
