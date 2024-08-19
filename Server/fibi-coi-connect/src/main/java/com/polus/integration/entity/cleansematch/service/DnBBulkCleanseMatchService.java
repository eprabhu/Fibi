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
import com.polus.integration.entity.cleansematch.dto.DnBCleanseMatchAPIResponse;
import com.polus.integration.entity.cleansematch.dto.DnBEntityCleanseMatchRequestDTO;
import com.polus.integration.entity.cleansematch.dto.DnBStageEntityMatchDTO;
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
		DnBCleanseMatchAPIResponse response =  new DnBCleanseMatchAPIResponse();
        for (StageDnBEntityMatch entityMatch : entityMatches) {
            if (stopMatchFlag.get()) {
                break;
            }
            try {
                String apiUrl = buildApiUrl(entityMatch);
                entityMatch.setRequest(apiUrl);
                response = callAPI(apiUrl);                
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
			DnBCleanseMatchAPIResponse response) {
		entityMatch.setResponse(response.getFullResponse().toString());
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

	private String setIntegrationStatus(DnBCleanseMatchAPIResponse res) {

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
	
    public List<DnBStageEntityMatchDTO> getCompletedRecord() {
        return dnbEntityMatchRepository.GetMatchCompleted();
    }

}
