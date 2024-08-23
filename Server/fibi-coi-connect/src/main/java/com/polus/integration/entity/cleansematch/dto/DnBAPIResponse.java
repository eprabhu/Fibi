package com.polus.integration.entity.cleansematch.dto;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@JsonIgnoreProperties(ignoreUnknown = true)
public class DnBAPIResponse {	
	private String HttpStatusCode;
    private TransactionDetail transactionDetail;
   // private InquiryDetail inquiryDetail;
    private int candidatesMatchedQuantity;
    private String matchDataCriteria;
    private List<MatchCandidate> matchCandidates;
    private APIError error;

    
    @Data
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class TransactionDetail {
        private String transactionID;
        private String transactionTimestamp;
        private String inLanguage;
        private String serviceVersion;
    }
    
    @Data
    @AllArgsConstructor
    @NoArgsConstructor
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class APIError {
    	private String errorCode;
        private String errorMessage;
        private List<ErrorDetail> errorDetails;
    }
    
    @Data
    @ToString
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class ErrorDetail {
    	private String parameter;
        private String description;        
    }
    
    @Data
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class InquiryDetail {
        private String name;
        private Address address;
    }

    @Data
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class Address {
        private String countryISOAlpha2Code;
        private String addressRegion;
    }

    @Data
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class DetailedAddress {
    	private CountryAddress addressCountry;
    	private LocalityAddress addressLocality;
    	private RegionAddress addressRegion;    	
    	private String postalCode;
    	private String postalCodeExtension;
    	private StreetAddress streetAddress;
    }    
    
    @Data
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class CountryAddress {   	
    	private String isoAlpha2Code;
    	private String name;
    }     
    
    @Data
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class LocalityAddress {  
    	private String name;
    }      
    
    @Data
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class RegionAddress {  
    	private String name;
    	private String abbreviatedName;
    }      

    @Data
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class StreetAddress {  
    	private String line1;
    	private String line2;
    }    
    
    @Data
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class MatchCandidate {
        private int displaySequence;
        private Organization organization;
        private MatchQualityInformation matchQualityInformation;
    }

    @Data
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class Organization {
        private String duns;
        private List<Website> websiteAddress;
        private DunsControlStatus dunsControlStatus;
        private String primaryName;
        private List<TradeNameStyle> tradeStyleNames;
        private List<Telephone> telephone;
        private DetailedAddress primaryAddress;
        private DetailedAddress mailingAddress;
        private List<RegistrationNumber> registrationNumbers;
        private List<Principal> mostSeniorPrincipals;
        private boolean isStandalone;
        private CorporateLinkage corporateLinkage;
    }
    
    @Data
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class TradeNameStyle {
        private String name;
        private Integer priority;
    }
 
    @Data
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class DunsControlStatus {
        private OperatingStatus operatingStatus;
        private boolean isMailUndeliverable;
    }
    
    @Data
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class Website {
        private String url;
        private String domainName;
    }

    @Data
    public static class OperatingStatus {
        private String description;
        private int dnbCode;
    }

    @Data
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class Telephone {
        private String telephoneNumber;
        private boolean isUnreachable;
    }

    @Data
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class RegistrationNumber {
        private String registrationNumber;
        private String typeDescription;
        private int typeDnBCode;
    }

    @Data
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class Principal {
        private String fullName;
    }

    @Data
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class CorporateLinkage {
        private List<FamilyTreeRole> familytreeRolesPlayed;
    }

    @Data
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class FamilyTreeRole {
        private String description;
        private int dnbCode;
    }

    @Data
    @JsonIgnoreProperties(ignoreUnknown = true)
    public static class MatchQualityInformation {
        private int confidenceCode;
//        private String matchGrade;
//        private int matchGradeComponentsCount;
//        private List<MatchGradeComponent> matchGradeComponents;
//        private String matchDataProfile;
//        private int matchDataProfileComponentsCount;
//        private List<MatchDataProfileComponent> matchDataProfileComponents;
//        private double nameMatchScore;
    }

//    @Data
//    public static class MatchGradeComponent {
//        private String componentType;
//        private String componentRating;
//
//        // Getters and setters
//    }
//
//    @Data
//    public static class MatchDataProfileComponent {
//        private String componentType;
//        private String componentValue;
//
//        // Getters and setters
//    }
}
