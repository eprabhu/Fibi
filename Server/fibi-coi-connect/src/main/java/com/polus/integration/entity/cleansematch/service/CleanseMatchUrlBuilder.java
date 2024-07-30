package com.polus.integration.entity.cleansematch.service;

import org.springframework.stereotype.Service;

import com.polus.integration.entity.cleansematch.entity.StageDnBEntityMatch;
import com.polus.integration.entity.config.APIConfig;

@Service
public class CleanseMatchUrlBuilder {
    private final APIConfig apiConfig;

    public CleanseMatchUrlBuilder(APIConfig apiConfig) {
        this.apiConfig = apiConfig;
    }

    public static class UrlBuilder {
        private final StringBuilder urlBuilder;
        private boolean hasParams = false;

        public UrlBuilder(String baseUrl) {
            this.urlBuilder = new StringBuilder(baseUrl);
        }

        public UrlBuilder addParam(String key, String value) {
            if (value != null && !value.isEmpty()) {
                if (hasParams) {
                    urlBuilder.append("&");
                } else {
                    urlBuilder.append("?");
                    hasParams = true;
                }
                urlBuilder.append(key).append("=").append(value);
            }
            return this;
        }

        public String build() {
            return urlBuilder.toString();
        }
    }

    public String buildApiUrl(StageDnBEntityMatch entityMatch) {
        String baseUrl = apiConfig.getCleansematch();

        if (entityMatch.getSourceDunsNumber() != null && !entityMatch.getSourceDunsNumber().isEmpty()) {
            return new UrlBuilder(baseUrl)
                    .addParam("duns", entityMatch.getSourceDunsNumber())
                    .build();
        } else {
            return new UrlBuilder(baseUrl)
                    .addParam("name", entityMatch.getSourceDataName())
                    .addParam("countryISOAlpha2Code", entityMatch.getCountryCode())
                    .addParam("streetAddressLine1", entityMatch.getAddressLine1())
                    .addParam("addressLocality", entityMatch.getAddressLine2())
                    .addParam("addressRegion", entityMatch.getState())
                    .addParam("postalCode", entityMatch.getPostalCode())
                    .build();
        }
    }
   
}
