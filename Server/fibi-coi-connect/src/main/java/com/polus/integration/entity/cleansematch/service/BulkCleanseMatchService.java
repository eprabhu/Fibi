package com.polus.integration.entity.cleansematch.service;

import java.time.LocalDateTime;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.polus.integration.entity.cleansematch.dto.BulkCleanseMatchAPIResponse;
import com.polus.integration.entity.cleansematch.dto.DnBCleanseMatchAPIResponse;
import com.polus.integration.entity.cleansematch.dto.DnBEntityCleanseMatchRequestDTO;
import com.polus.integration.entity.cleansematch.dto.DnBStageEntityMatchDTO;
import com.polus.integration.entity.cleansematch.dto.DnBCleanseMatchAPIResponse.ErrorDetail;
import com.polus.integration.entity.cleansematch.entity.DnBEntityMatchRepository;
import com.polus.integration.entity.cleansematch.entity.StageDnBEntityMatch;
import com.polus.integration.entity.config.Constants;
import com.polus.integration.entity.config.ErrorCode;

@Service
public class BulkCleanseMatchService {

	@Autowired
	private DnBEntityMatchRepository dnbEntityMatchRepository;

	@Autowired
	private CleanseMatchUrlBuilder urlBuilder;

	@Autowired
	private DnBCleanseMatchAPIService apiService;

	private volatile AtomicBoolean stopMatchFlag = new AtomicBoolean(false);

	private static final ObjectMapper objectMapper = new ObjectMapper();

	@Async
	public CompletableFuture<Void> startBulkCleanseMatch() {
		Pageable firstPageWithTenRecords = PageRequest.of(0, 1);
		List<StageDnBEntityMatch> entityMatches = dnbEntityMatchRepository.findAll(firstPageWithTenRecords)
				.getContent();
		// List<StageDnBEntityMatch> entityMatches = dnbEntityMatchRepository.findAll();
		BulkCleanseMatchAPIResponse response = new BulkCleanseMatchAPIResponse();
		DnBCleanseMatchAPIResponse apiResponse = new DnBCleanseMatchAPIResponse();
		for (StageDnBEntityMatch entityMatch : entityMatches) {
			try {
				if (stopMatchFlag.get()) {
					break;
				}
				try {
					String apiUrl = buildApiUrl(entityMatch);
					entityMatch.setRequest(apiUrl);
					apiResponse = callAPI(apiUrl);
					response = PrepareResponse(apiResponse);
				} catch (Exception e) {
					ErrorCode errorCode = ErrorCode.DNB_BULK_MATCH_ERROR;
					entityMatch.setErrorCode(errorCode.getErrorCode());
					entityMatch.setErrorMessage(errorCode.getErrorMessage());
					entityMatch.setErrorDetails(e.getMessage());
					entityMatch.setUpdateTimestamp(LocalDateTime.now());
					entityMatch.setIntegrationStatusCode(Constants.INT_STATUS_ERROR);
				} finally {
					entityMatch = prepareDBSaveObject(entityMatch, response);
					dnbEntityMatchRepository.save(entityMatch);
				}
			} catch (Exception e) {
				e.printStackTrace();
			}
		}

		return CompletableFuture.completedFuture(null);
	}

	public void stopBulkCleanseMatch() {
		stopMatchFlag.set(true);
	}

	private String buildApiUrl(StageDnBEntityMatch sponsorMatch) {
		DnBEntityCleanseMatchRequestDTO dto = new DnBEntityCleanseMatchRequestDTO();
		dto.setSourceDunsNumber(sponsorMatch.getSourceDunsNumber());
		dto.setSourceDataName(sponsorMatch.getSourceDataName());
		dto.setCountryCode(sponsorMatch.getCountryCode());
		dto.setAddressLine1(sponsorMatch.getAddressLine1());
		dto.setAddressLine2(sponsorMatch.getAddressLine2());
		dto.setState(sponsorMatch.getState());
		dto.setPostalCode(sponsorMatch.getPostalCode());
		return urlBuilder.buildApiUrl(dto);
	}

	private DnBCleanseMatchAPIResponse callAPI(String apiUrl) {
		return apiService.callAPI(apiUrl);
	}

	private StageDnBEntityMatch prepareDBSaveObject(StageDnBEntityMatch entityMatch,
			BulkCleanseMatchAPIResponse response) {

		if (response.getFullResponse() != null) {
			try {
				String fullResponse = objectMapper.writeValueAsString(response.getFullResponse());
				entityMatch.setResponse(fullResponse);
			} catch (JsonProcessingException e) {
				e.printStackTrace();
			}

		}
		if (response.getHighestMatch() != null) {

			try {
				String bestMatch = objectMapper.writeValueAsString(response.getHighestMatch());
				entityMatch.setBestMatchResult(bestMatch);
			} catch (JsonProcessingException e) {
				e.printStackTrace();
			}

		}
		entityMatch.setBestMatchConfidenceCode(response.getHighestMatchConfidenceCode());
		entityMatch.setExternalSysTransactionId(response.getTransactionID());
		entityMatch.setHttpStatusCode(response.getHttpStatusCode());
		entityMatch.setCandidateMatchedQuantity(response.getCandidatesMatchedQuantity());

		if (response.getMatchCandidates() != null) {
			try {
				String matchResult = objectMapper.writeValueAsString(response.getMatchCandidates());
				entityMatch.setMatchedResults(matchResult);
			} catch (JsonProcessingException e) {
				e.printStackTrace();
			}
		}

		entityMatch.setErrorCode(response.getErrorCode());
		entityMatch.setErrorMessage(response.getErrorMessage());
		entityMatch.setErrorDetails(response.getErrorDetails());
		entityMatch.setUpdateTimestamp(LocalDateTime.now());
		entityMatch.setIntegrationStatusCode(setIntegrationStatus(response));

		return entityMatch;
	}

	private String setIntegrationStatus(BulkCleanseMatchAPIResponse res) {

		String status = Constants.INT_STATUS_ERROR;
		if (res.getHttpStatusCode() != null && res.getHttpStatusCode().equals(Constants.HTTP_SUCCESS_CODE)) {
			if (res.getHighestMatch() != null) {
				status = Constants.INT_STATUS_SUCCESSFUL_AND_MATCHED;
			} else {
				status = Constants.INT_STATUS_SUCCESSFUL_AND_NO_MATCH;
			}
		}
		return status;
	}

	public List<DnBStageEntityMatchDTO> getCompletedRecord() {
		return dnbEntityMatchRepository.GetMatchCompleted();
	}

	private BulkCleanseMatchAPIResponse PrepareResponse(DnBCleanseMatchAPIResponse apiResponse) {
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
						response.setErrorMessage(apiResponse.getError().getErrorMessage());
						List<ErrorDetail> errorDetails = apiResponse.getError().getErrorDetails();
						if (errorDetails != null) {
							response.setErrorDetails(
									errorDetails.stream().map(ErrorDetail::toString).collect(Collectors.joining("; ")));
						}
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
}
