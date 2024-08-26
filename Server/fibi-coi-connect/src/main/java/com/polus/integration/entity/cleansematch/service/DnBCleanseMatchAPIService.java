package com.polus.integration.entity.cleansematch.service;

import java.util.List;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatusCode;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.reactive.function.client.WebClient;
import org.springframework.web.reactive.function.client.WebClientResponseException;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.polus.integration.entity.apitokenservice.TokenService;
import com.polus.integration.entity.cleansematch.config.ErrorCode;
import com.polus.integration.entity.cleansematch.dto.BulkCleanseMatchAPIResponse;
import com.polus.integration.entity.cleansematch.dto.DnBAPIResponse;
import com.polus.integration.entity.cleansematch.dto.DnBAPIResponse.APIError;
import com.polus.integration.entity.cleansematch.dto.DnBAPIResponse.ErrorDetail;
import com.polus.integration.entity.cleansematch.dto.EntityCleanseMatchAPIResponse;

@Service
public class DnBCleanseMatchAPIService {

	@Autowired
	private WebClient.Builder webClientBuilder;

	@Autowired
	private TokenService tokenService;

	public EntityCleanseMatchAPIResponse callAPI(String apiUrl) {
		String token = tokenService.getToken();
		DnBAPIResponse apiResponse = new DnBAPIResponse();
		apiResponse = callExternalAPI(apiUrl, token);
		BulkCleanseMatchAPIResponse response = PrepareResponse(apiResponse);
		return  EntityCleanseMatchAPIResponse.builder()
											 .httpStatusCode(response.getHttpStatusCode())
											 .transactionID(response.getTransactionID())
											 .candidatesMatchedQuantity(response.getCandidatesMatchedQuantity())
											 .matchCandidates(response.getMatchCandidates())
											 .errorCode(response.getErrorCode())
											 .errorMessage(response.getErrorMessage())
											 .errorDetails(response.getErrorDetails())
											 .build();										 
											 
										 
	}
	
	public BulkCleanseMatchAPIResponse callAPIForBulk(String apiUrl) {
		String token = tokenService.getToken();
		DnBAPIResponse apiResponse = new DnBAPIResponse();
		apiResponse = callExternalAPI(apiUrl, token);
		BulkCleanseMatchAPIResponse response = PrepareResponse(apiResponse);
		return response;
	}

	private BulkCleanseMatchAPIResponse PrepareResponse(DnBAPIResponse apiResponse) {
		BulkCleanseMatchAPIResponse response = new BulkCleanseMatchAPIResponse();
		try {

			if (apiResponse != null) {
				response.setHttpStatusCode(apiResponse.getHttpStatusCode());
				response.setFullResponse(apiResponse);
				if (apiResponse.getTransactionDetail() != null) {
					response.setTransactionID(apiResponse.getTransactionDetail().getTransactionID());
				}
				response.setCandidatesMatchedQuantity(apiResponse.getCandidatesMatchedQuantity());
				if (apiResponse.getMatchCandidates() != null && !apiResponse.getMatchCandidates().isEmpty()) {
					response.setMatchCandidates(apiResponse.getMatchCandidates());
					if (apiResponse.getMatchCandidates().get(0) != null) {
						response.setHighestMatch(apiResponse.getMatchCandidates().get(0));

						if (apiResponse.getMatchCandidates().get(0).getMatchQualityInformation() != null) {
							response.setHighestMatchConfidenceCode(apiResponse.getMatchCandidates().get(0)
									.getMatchQualityInformation().getConfidenceCode());
						}
					}
				}

				if (apiResponse.getError() != null) {

					if (apiResponse.getError().getErrorCode() != null) {
						response.setErrorCode(apiResponse.getError().getErrorCode());
						response.setErrorDetails(apiResponse.getError().getErrorMessage());
						List<ErrorDetail> errorDetails = apiResponse.getError().getErrorDetails();
						response.setErrorDetails(
								errorDetails
									.stream()
									.map(ErrorDetail::toString)
									.collect(Collectors.joining("; ")));
					}

				}
			}

		} catch (Exception e) {
			ErrorCode errorCode = ErrorCode.DNB_CLEANSE_MATCH_ERROR;
			response.setErrorCode(errorCode.getErrorCode());
			response.setErrorMessage("Error while API PrepareResponse for Cleanse Match");
			response.setErrorDetails(e.getMessage());
			response.setErrorCode(null);
		}
		return response;
	}

	private DnBAPIResponse callExternalAPI(String apiUrl, String token) {
		DnBAPIResponse response = new DnBAPIResponse();
		ErrorCode errorCode = ErrorCode.DNB_CLEANSE_MATCH_API_INVOKE;
		try {

			ResponseEntity<String> responseEntity = webClientBuilder.build().get().uri(apiUrl)
					.header("Authorization", token).retrieve().toEntity(String.class).block();

			if (responseEntity != null) {
				String responseBody = responseEntity.getBody();
				HttpStatusCode httpStatus = responseEntity.getStatusCode();
				ObjectMapper mapper = new ObjectMapper();
				mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
				response = mapper.readValue(responseBody, DnBAPIResponse.class);
				response.setHttpStatusCode(String.valueOf(httpStatus.value()));
			} else {
				response.setError(new APIError(errorCode.getErrorCode(), "No response received from the API", null));
			}
		} catch (WebClientResponseException e) {
			response = handleWebClientException(e);
		} catch (Exception e) {
			response.setError(new APIError(errorCode.getErrorCode(), e.getMessage(), null));
		}

		return response;
	}

	private DnBAPIResponse handleWebClientException(WebClientResponseException e) {
		ObjectMapper mapper = new ObjectMapper();
		DnBAPIResponse response = new DnBAPIResponse();
		mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
		try {
			response = mapper.readValue(e.getResponseBodyAsString(), DnBAPIResponse.class);
		} catch (JsonMappingException e1) {
			e1.printStackTrace();
		} catch (JsonProcessingException e1) {
			e1.printStackTrace();
		}
		response.setHttpStatusCode(String.valueOf(e.getStatusCode().value()));
		return response;
	}
}
