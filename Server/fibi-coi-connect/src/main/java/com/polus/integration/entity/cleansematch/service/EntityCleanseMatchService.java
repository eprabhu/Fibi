package com.polus.integration.entity.cleansematch.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.polus.integration.entity.cleansematch.config.ErrorCode;
import com.polus.integration.entity.cleansematch.dto.DnBEntityCleanseMatchRequestDTO;
import com.polus.integration.entity.cleansematch.dto.EntityCleanseMatchAPIResponse;

@Service
public class EntityCleanseMatchService {

	@Autowired
	private CleanseMatchUrlBuilder urlBuilder;

	@Autowired
	private DnBCleanseMatchAPIService apiService;

	public EntityCleanseMatchAPIResponse runCleanseMatch(DnBEntityCleanseMatchRequestDTO request) {
		EntityCleanseMatchAPIResponse response = new EntityCleanseMatchAPIResponse();
		try {
			String apiUrl = buildApiUrl(request);
			response = callAPI(apiUrl);
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

	private EntityCleanseMatchAPIResponse callAPI(String apiUrl) {
		return apiService.callAPI(apiUrl);
	}

}
