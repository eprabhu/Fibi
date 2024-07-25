package com.polus.integration.entity.apitokenservice;

import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.util.Base64;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.polus.integration.entity.config.APIConfig;

@Component
public class DnBAPITokenClient implements ThirdPartyApiClient{

	@Autowired
	private APIConfig urlConfig;
	
	@Override
	public TokenResponseDTO getNewToken() {
		try {
            // API endpoint
            String endpoint = "https://plus.dnb.com/v3/token";//
            //"https://plus.dnb.com/v3/token"; //
            // Username and password
            String username = "";
            String password = "";
            
            // Encode username and password in Base64
            String auth = username + ":" + password;
            String encodedAuth = Base64.getEncoder().encodeToString(auth.getBytes(StandardCharsets.UTF_8));

            // Create HttpClient
            HttpClient client = HttpClient.newHttpClient();

            // Create HttpRequest
            HttpRequest request = HttpRequest.newBuilder()
                    .uri(URI.create(endpoint))
                    .header("Content-Type", "application/x-www-form-urlencoded")
                    .header("Authorization", "Basic " + encodedAuth)
                    .header("Cache-Control", "no-cache")
                    .POST(HttpRequest.BodyPublishers.ofString("grant_type=client_credentials"))
                    .build();

            // Send the request and get the response
            HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());

            ObjectMapper objectMapper = new ObjectMapper();
            
            // Convert JSON string to TokenResponseDTO object
            TokenResponseDTO tokenResponse = objectMapper.readValue(response.body(), TokenResponseDTO.class);
            System.out.println("Access Token: " + tokenResponse.getAccessToken());
            // Print the response code
            System.out.println("Response Code: " + response.statusCode());

            // Print the response body
            System.out.println("Response Body: " + response.body());
            
            return tokenResponse;

        } catch (Exception e) {
            e.printStackTrace();
        }
		return new TokenResponseDTO();
	}
	
	private String getAuthTokenURL() {
		return urlConfig.getAuthToken();
	}

}
