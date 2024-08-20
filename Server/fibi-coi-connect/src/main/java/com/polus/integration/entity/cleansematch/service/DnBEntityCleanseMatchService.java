package com.polus.integration.entity.cleansematch.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.polus.integration.entity.cleansematch.config.ErrorCode;
import com.polus.integration.entity.cleansematch.dto.DnBCleanseMatchAPIResponse;
import com.polus.integration.entity.cleansematch.dto.DnBEntityCleanseMatchRequestDTO;

@Service
public class DnBEntityCleanseMatchService {

	@Autowired
	private CleanseMatchUrlBuilder urlBuilder;

	@Autowired
	private CleanseMatchAPIService apiService;

	public DnBCleanseMatchAPIResponse runCleanseMatch(DnBEntityCleanseMatchRequestDTO request) {
		DnBCleanseMatchAPIResponse response = new DnBCleanseMatchAPIResponse();
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

	public String runCleanseMatchWithRawResponse(DnBEntityCleanseMatchRequestDTO request) {
		String response = "";
		try {
			String apiUrl = buildApiUrl(request);
			response = callAPIRawResponse(apiUrl);
		} catch (Exception e) {
			e.printStackTrace();
		}

		return response;
	}
	private String buildApiUrl(DnBEntityCleanseMatchRequestDTO request) {
		return urlBuilder.buildApiUrl(request);
	}

	private DnBCleanseMatchAPIResponse callAPI(String apiUrl) {
		return apiService.callAPI(apiUrl);
	}
	private String callAPIRawResponse(String apiUrl) {
		return apiService.callAPIRawResponse(apiUrl);
	}
}
