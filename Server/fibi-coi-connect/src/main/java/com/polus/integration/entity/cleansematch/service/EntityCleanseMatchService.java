package com.polus.integration.entity.cleansematch.service;

import java.util.List;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.polus.integration.entity.cleansematch.dao.EntityCleanseMatchDAO;
import com.polus.integration.entity.cleansematch.dto.DnBCleanseMatchAPIResponse;
import com.polus.integration.entity.cleansematch.dto.DnBCleanseMatchAPIResponse.ErrorDetail;
import com.polus.integration.entity.cleansematch.dto.DnBCleanseMatchAPIResponse.MatchCandidate;
import com.polus.integration.entity.cleansematch.dto.DnBEntityCleanseMatchRequestDTO;
import com.polus.integration.entity.cleansematch.dto.EntityCleanseMatchAPIResponse;
import com.polus.integration.entity.cleansematch.dto.EntityInfoDTO;
import com.polus.integration.entity.config.ErrorCode;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class EntityCleanseMatchService {

	@Autowired
	private CleanseMatchUrlBuilder urlBuilder;

	@Autowired
	private DnBCleanseMatchAPIService apiService;
	
	@Autowired
	private EntityCleanseMatchDAO dao;

	public EntityCleanseMatchAPIResponse runCleanseMatch(DnBEntityCleanseMatchRequestDTO request) {
		EntityCleanseMatchAPIResponse response = new EntityCleanseMatchAPIResponse();		 
		try {
			String apiUrl = buildApiUrl(request);
			DnBCleanseMatchAPIResponse apiResponse = callAPI(apiUrl);
			response = PrepareResponse(apiResponse);
		} catch (Exception e) {
			ErrorCode errorCode = ErrorCode.DNB_CLEANSE_MATCH_ERROR;
			response.setErrorCode(errorCode.getErrorCode());
			response.setErrorMessage(errorCode.getErrorMessage());
			response.setErrorDetails(e.getMessage());
		}

		return response;
	}

	private String buildApiUrl(DnBEntityCleanseMatchRequestDTO request) {
		return urlBuilder.buildApiUrl(request);
	}

	private DnBCleanseMatchAPIResponse callAPI(String apiUrl) {
		return apiService.callAPI(apiUrl);
	}

	private EntityCleanseMatchAPIResponse PrepareResponse(DnBCleanseMatchAPIResponse apiResponse) {
		EntityCleanseMatchAPIResponse response = new EntityCleanseMatchAPIResponse();
		try {

			if (apiResponse != null) {
				response.setHttpStatusCode(apiResponse.getHttpStatusCode());
				if (apiResponse.getTransactionDetail() != null) {
					response.setTransactionID(apiResponse.getTransactionDetail().getTransactionID());
				}
				response.setCandidatesMatchedQuantity(apiResponse.getCandidatesMatchedQuantity());
				if (apiResponse.getMatchCandidates() != null && !apiResponse.getMatchCandidates().isEmpty()) {
					response.setMatchCandidates(apiResponse.getMatchCandidates());	
					response = CheckEntityExistenceByDUNS(response);
				}

				if (apiResponse.getError() != null) {

					if (apiResponse.getError().getErrorCode() != null) {
						response.setErrorCode(apiResponse.getError().getErrorCode());
						response.setErrorMessage(apiResponse.getError().getErrorMessage());
						List<ErrorDetail> errorDetails = apiResponse.getError().getErrorDetails();
						if(errorDetails != null) {
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
			ErrorCode errorCode = ErrorCode.DNB_CLEANSE_MATCH_ERROR;
			response.setErrorCode(errorCode.getErrorCode());
			response.setErrorMessage("Error while API PrepareResponse for Cleanse Match");
			response.setErrorDetails(e.getMessage());
		}
		
		
		return response;
	}

	private EntityCleanseMatchAPIResponse CheckEntityExistenceByDUNS(EntityCleanseMatchAPIResponse response) {

		try {
			response.getMatchCandidates().parallelStream().forEach( data -> {
				if (DunsIsEmpty(data)) {
					return;
				}
	
				String dunsNumber = data.getOrganization().getDuns();
	
				EntityInfoDTO entityInfo = dao.getEntityInfoByDUNS(dunsNumber);
				data.setEntity(entityInfo);	
				
			});
			
		}catch(Exception e) {
			log.error("CLEANSE MATCH API: Exception while CheckEntityExistenceByDUNS, error "+ e.getMessage());
		}

		return response;
	}

	private boolean DunsIsEmpty(MatchCandidate data) {
		if(data == null) {
			return true;
		}
		
		if(data.getOrganization() == null) {
			return true;
		}
		
		if(data.getOrganization().getDuns() == null) {
			return true;
		}
		return false;
	}
	
	
}
