package com.polus.integration.entity.cleansematch.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.reactive.function.client.WebClient;
import org.springframework.web.reactive.function.client.WebClientResponseException;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.polus.integration.entity.apitokenservice.TokenService;
import com.polus.integration.entity.cleansematch.config.Constants;
import com.polus.integration.entity.cleansematch.config.ErrorCode;
import com.polus.integration.entity.cleansematch.dto.DnBCleanMatchAPIResponse;

import reactor.core.publisher.Mono;

@Service
public class CleanseMatchAPIService {


    @Autowired
    private WebClient.Builder webClientBuilder;

    @Autowired
    private TokenService tokenService;

    public DnBCleanMatchAPIResponse callAPI(String apiUrl) {
        String token = tokenService.getToken();
        DnBCleanMatchAPIResponse response = new DnBCleanMatchAPIResponse();
        ObjectMapper objectMapper = new ObjectMapper().enable(SerializationFeature.INDENT_OUTPUT);

        try {
            String jsonResponse = callExternalAPI(apiUrl, token, response, objectMapper);

            if (jsonResponse != null) {
                processSuccessResponse(jsonResponse, response, objectMapper);
            }

        } catch (Exception e) {        	
            System.err.println("Exception occurred: " + e.getMessage());
        }

        return response;
    }

    private String callExternalAPI(String apiUrl, String token, DnBCleanMatchAPIResponse response, ObjectMapper objectMapper) {
        return webClientBuilder.build()
            .get()
            .uri(apiUrl)
            .header("Authorization", token)
            .retrieve()
            .onStatus(status -> status.is4xxClientError() || status.is5xxServerError(), clientResponse -> 
                clientResponse.bodyToMono(String.class).flatMap(errorBody -> {
                    handleErrorResponse(clientResponse.statusCode().toString(), errorBody, response, objectMapper);
                    return Mono.error(new RuntimeException("Error response: " + errorBody));
                })
            )
            .bodyToMono(String.class)
            .doOnSuccess(body -> {
                response.setHttpStatusCode(Constants.HTTP_SUCCESS_CODE);
                response.setFullResponse(body);
            })
            .doOnError(e -> handleWebClientException(e, response, objectMapper))
            .block();
    }

    private void handleErrorResponse(String statusCode, String errorBody, DnBCleanMatchAPIResponse response, ObjectMapper objectMapper) {
        response.setFullResponse(errorBody);
        response.setHttpStatusCode(statusCode);
        parseErrorResponse(errorBody, response, objectMapper);
    }

    private void handleWebClientException(Throwable e, DnBCleanMatchAPIResponse response, ObjectMapper objectMapper) {
        if (e instanceof WebClientResponseException) {
            WebClientResponseException webClientResponseException = (WebClientResponseException) e;
            response.setHttpStatusCode(webClientResponseException.getStatusCode().toString());
            response.setFullResponse(webClientResponseException.getResponseBodyAsString());
            parseErrorResponse(webClientResponseException.getResponseBodyAsString(), response, objectMapper);
        } else {
            response.setFullResponse(e.getMessage());
        }
    }

    private void processSuccessResponse(String jsonResponse, DnBCleanMatchAPIResponse response, ObjectMapper objectMapper) throws Exception {
        JsonNode rootNode = objectMapper.readTree(jsonResponse);
        response.setFullResponse(jsonResponse);

        if (rootNode.has("transactionDetail")) {
            JsonNode transactionDetail = rootNode.get("transactionDetail");
            if (transactionDetail.has("transactionID")) {
                response.setTransactionID(transactionDetail.get("transactionID").asText());
            }
        }

        if (rootNode.has("candidatesMatchedQuantity")) {
            response.setCandidatesMatchedQuantity(rootNode.get("candidatesMatchedQuantity").asInt());
        }

        if (rootNode.has("matchCandidates")) {
            response.setMatchCandidates(objectMapper.writeValueAsString(rootNode.get("matchCandidates")));

            if (rootNode.get("matchCandidates").isArray() && rootNode.get("matchCandidates").size() > 0) {
                JsonNode highestMatchNode = rootNode.get("matchCandidates").get(0);
                response.setHighestMatch(objectMapper.writeValueAsString(highestMatchNode));

                if (highestMatchNode.has("matchQualityInformation")) {
                    JsonNode matchQualityInfo = highestMatchNode.get("matchQualityInformation");
                    if (matchQualityInfo.has("confidenceCode")) {
                        response.setHighestMatchConfidenceCode(matchQualityInfo.get("confidenceCode").asInt());
                    }
                }
            }
        }
    }

    private void parseErrorResponse(String errorBody, DnBCleanMatchAPIResponse response, ObjectMapper objectMapper) {
        try {
            JsonNode rootNode = objectMapper.readTree(errorBody);
            
            if (rootNode.has("transactionDetail")) {
                JsonNode transactionDetail = rootNode.get("transactionDetail");
                if (transactionDetail.has("transactionID")) {
                    response.setTransactionID(transactionDetail.get("transactionID").asText());
                }
            }
            
            if (rootNode.has("error")) {
                JsonNode error = rootNode.get("error");
                if (error.has("errorCode")) {
                    response.setErrorCode(error.get("errorCode").asText());
                }
                if (error.has("errorMessage")) {
                    response.setErrorMessage(error.get("errorMessage").asText());
                }
                if (error.has("errorDetails") && error.get("errorDetails").isArray() && error.get("errorDetails").size() > 0) {
                    JsonNode errorDetailNode = error.get("errorDetails").get(0);
                    StringBuilder errorDetailBuilder = new StringBuilder();
                    if (errorDetailNode.has("parameter")) {
                        errorDetailBuilder.append(errorDetailNode.get("parameter").asText()).append(": ");
                    }
                    if (errorDetailNode.has("description")) {
                        errorDetailBuilder.append(errorDetailNode.get("description").asText());
                    }
                    response.setErrorDetails(errorDetailBuilder.toString());
                }
            }
        } catch (Exception e) {
            System.err.println("Error parsing error response: " + e.getMessage());
        }
    }
}
