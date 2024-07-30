package com.polus.integration.entity.cleansematch.service;

import java.time.LocalDateTime;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.atomic.AtomicBoolean;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import com.polus.integration.entity.cleansematch.config.Constants;
import com.polus.integration.entity.cleansematch.config.ErrorCode;
import com.polus.integration.entity.cleansematch.dto.DnBCleanMatchAPIResponse;
import com.polus.integration.entity.cleansematch.dto.StageDnBEntityMatchDTO;
import com.polus.integration.entity.cleansematch.entity.DnBEntityMatchRepository;
import com.polus.integration.entity.cleansematch.entity.StageDnBEntityMatch;

@Service
public class DnBBulkCleanseMatchService {

	@Autowired
	private DnBEntityMatchRepository dnbEntityMatchRepository;

	@Autowired
	private CleanseMatchUrlBuilder urlBuilder;

	@Autowired
	private CleanseMatchAPIService apiService;

	private volatile AtomicBoolean stopMatchFlag = new AtomicBoolean(false);

	@Async
	public CompletableFuture<Void> startBulkCleanseMatch() {
		Pageable firstPageWithTenRecords = PageRequest.of(0, 1000);
		List<StageDnBEntityMatch> entityMatches = dnbEntityMatchRepository.findAll(firstPageWithTenRecords)
				.getContent();
		//List<StageDnBEntityMatch> entityMatches = dnbEntityMatchRepository.findAll();
		DnBCleanMatchAPIResponse response =  new DnBCleanMatchAPIResponse();
        for (StageDnBEntityMatch entityMatch : entityMatches) {
            if (stopMatchFlag.get()) {
                break;
            }
            try {
                String apiUrl = buildApiUrl(entityMatch);
                entityMatch.setRequest(apiUrl);
                response = callApi(apiUrl);                
            } catch (Exception e) {                
                ErrorCode errorCode = ErrorCode.DNB_BULK_MATCH_ERROR;
                entityMatch.setErrorCode(errorCode.getErrorCode());
                entityMatch.setErrorMessage(errorCode.getErrorMessage());
                entityMatch.setErrorDetails(e.getMessage());
                entityMatch.setUpdateTimestamp(LocalDateTime.now());
                entityMatch.setIntegrationStatusCode(Constants.INT_STATUS_ERROR);                
            }finally{            	
            	entityMatch = prepareDBSaveObject(entityMatch, response);                			  
                dnbEntityMatchRepository.save(entityMatch);
            }
        }

		return CompletableFuture.completedFuture(null);
	}

	public void stopBulkCleanseMatch() {
    	stopMatchFlag.set(true);
    }
    
	private String buildApiUrl(StageDnBEntityMatch sponsorMatch) {
		return urlBuilder.buildApiUrl(sponsorMatch);
	}

	private DnBCleanMatchAPIResponse callApi(String apiUrl) {
		return apiService.callAPI(apiUrl);
	}

	private StageDnBEntityMatch prepareDBSaveObject(StageDnBEntityMatch entityMatch,
			DnBCleanMatchAPIResponse response) {
		entityMatch.setResponse(response.getFullResponse());
		entityMatch.setBestMatchResult(response.getHighestMatch());
		entityMatch.setBestMatchConfidenceCode(response.getHighestMatchConfidenceCode());
		entityMatch.setExternalSysTransactionId(response.getTransactionID());
		entityMatch.setHttpStatusCode(response.getHttpStatusCode());
		entityMatch.setCandidateMatchedQuantity(response.getCandidatesMatchedQuantity());
		entityMatch.setMatchedResults(response.getMatchCandidates());
		entityMatch.setErrorCode(response.getErrorCode());
		entityMatch.setErrorMessage(response.getErrorMessage());
		entityMatch.setErrorDetails(response.getErrorDetails());
		entityMatch.setUpdateTimestamp(LocalDateTime.now());
		entityMatch.setIntegrationStatusCode(setIntegrationStatus(response));

		return entityMatch;
	}

	private String setIntegrationStatus(DnBCleanMatchAPIResponse res) {

		String status = Constants.INT_STATUS_ERROR;
		if (res.getHttpStatusCode().equals(Constants.HTTP_SUCCESS_CODE)) {
			if (!res.getHighestMatch().isEmpty()) {
				status = Constants.INT_STATUS_SUCCESSFUL_AND_MATCHED;
			} else {
				status = Constants.INT_STATUS_SUCCESSFUL_AND_NO_MATCH;
			}
		}
		return status;
	}
	
    public List<StageDnBEntityMatchDTO> getCompletedRecord() {
        return dnbEntityMatchRepository.GetMatchCompleted();
    }

}
