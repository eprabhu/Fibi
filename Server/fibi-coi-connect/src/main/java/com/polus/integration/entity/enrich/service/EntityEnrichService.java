package com.polus.integration.entity.enrich.service;

import java.util.List;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.polus.integration.entity.config.ErrorCode;
import com.polus.integration.entity.enrich.dto.DnBEnrichAPIResponse;
import com.polus.integration.entity.enrich.dto.DnBEntityEnrichRequestDTO;
import com.polus.integration.entity.enrich.dto.EntityEnrichAPIResponse;
import com.polus.integration.entity.enrich.dto.DnBEnrichAPIResponse.ErrorDetail;
import com.polus.integration.entity.enrich.dto.DnBEnrichAPIResponse.Organization;

@Service
public class EntityEnrichService {

	@Autowired
	private EnrichUrlBuilder urlBuilder;

	@Autowired
	private DnBEnrichAPIService apiService;

	public EntityEnrichAPIResponse runEnrich(DnBEntityEnrichRequestDTO request) {
		EntityEnrichAPIResponse response = new EntityEnrichAPIResponse();
		try {
			String apiUrl = buildApiUrl(request);
			DnBEnrichAPIResponse dnbResponse = callAPI(apiUrl);
			response = PrepareResponse(dnbResponse);
		} catch (Exception e) {
			ErrorCode errorCode = ErrorCode.DNB_ENRICH_ERROR;
			response.setErrorCode(errorCode.getErrorCode());
			response.setErrorMessage(errorCode.getErrorMessage());
			response.setErrorDetails(e.getMessage());
		}

		return response;
	}

	private String buildApiUrl(DnBEntityEnrichRequestDTO request) {
		return urlBuilder.buildApiUrl(request);
	}

	private DnBEnrichAPIResponse callAPI(String apiUrl) {
		return apiService.callAPI(apiUrl);
	}
	

	private EntityEnrichAPIResponse PrepareResponse(DnBEnrichAPIResponse apiResponse) {
		EntityEnrichAPIResponse response = new EntityEnrichAPIResponse();
		try {

			if (apiResponse != null) {
				response.setHttpStatusCode(apiResponse.getHttpStatusCode());
				
				if (apiResponse.getTransactionDetail() != null) {
					response.setTransactionID(apiResponse.getTransactionDetail().getTransactionID());
				}
				
				if (apiResponse.getOrganization() != null ) {
					response.setOrganization(apiResponse.getOrganization());
					
				}

				if (apiResponse.getError() != null) {

					if (apiResponse.getError().getErrorCode() != null) {
						response.setErrorCode(apiResponse.getError().getErrorCode());
						response.setErrorMessage(apiResponse.getError().getErrorMessage());
						if(apiResponse.getError().getErrorDetails() != null) {
							
							List<ErrorDetail> errorDetails = apiResponse.getError().getErrorDetails();
							
							response.setErrorDetails(
									errorDetails
										.stream()
										.map(ErrorDetail::toString)
										.collect(Collectors.joining("; ")));
							
						}
						
					}

				}
			}

		} catch (Exception e) {
			ErrorCode errorCode = ErrorCode.DNB_ENRICH_ERROR;
			response.setErrorCode(errorCode.getErrorCode());
			response.setErrorMessage("Error while API PrepareResponse for Cleanse Match");
			response.setErrorDetails(e.getMessage());
			response.setErrorCode(null);
		}
		return response;
	}
	
	
	private String getUEI(Organization organization) {
		return null;
		
	}
	
	private Integer getNoOfEmployees(Organization organization) {
		return null;		
	}
	
	private String getCompanyProfile(Organization organization) {
		return null;
		
	}
	
	private boolean isPubliclyTradedCompany(Organization organization) {
		return false;
	}
	
	private String getForiegnName(Organization organization) {
		return null;
	}
	
}
