package com.polus.fibicomp.orcid.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({
    "external-id-type",
    "external-id-value",
    "external-id-normalized",
    "external-id-normalized-error",
    "external-id-url",
    "external-id-relationship"
})
public class ExternalIdDto {

	@JsonProperty("external-id-type")
    private String externalIdType;

    @JsonProperty("external-id-value")
    private String externalIdValue;
   
    @JsonProperty("external-id-normalized")
    private ExternalIdNormalizedDto externalIdNormalized;

    @JsonProperty("external-id-normalized-error")
    private ExternalIdNormalizedErrorDto externalIdNormalizedError;

    @JsonProperty("external-id-url")
    private ExternalIdUrlDto externalIdUrl;

    @JsonProperty("external-id-relationship")
    private String externalIdRelationship;

    @JsonProperty("external-id-type")
	public String getExternalIdType() {
		return externalIdType;
	}

    @JsonProperty("external-id-type")
	public void setExternalIdType(String externalIdType) {
		this.externalIdType = externalIdType;
	}

    @JsonProperty("external-id-value")
	public String getExternalIdValue() {
		return externalIdValue;
	}

    @JsonProperty("external-id-value")
	public void setExternalIdValue(String externalIdValue) {
		this.externalIdValue = externalIdValue;
	}

    @JsonProperty("external-id-normalized")
	public ExternalIdNormalizedDto getExternalIdNormalized() {
		return externalIdNormalized;
	}

    @JsonProperty("external-id-normalized")
	public void setExternalIdNormalized(ExternalIdNormalizedDto externalIdNormalized) {
		this.externalIdNormalized = externalIdNormalized;
	}

    @JsonProperty("external-id-normalized-error")
	public ExternalIdNormalizedErrorDto getExternalIdNormalizedError() {
		return externalIdNormalizedError;
	}

    @JsonProperty("external-id-normalized-error")
	public void setExternalIdNormalizedError(ExternalIdNormalizedErrorDto externalIdNormalizedError) {
		this.externalIdNormalizedError = externalIdNormalizedError;
	}

    @JsonProperty("external-id-url")
	public ExternalIdUrlDto getExternalIdUrl() {
		return externalIdUrl;
	}

    @JsonProperty("external-id-url")
	public void setExternalIdUrl(ExternalIdUrlDto externalIdUrl) {
		this.externalIdUrl = externalIdUrl;
	}

    @JsonProperty("external-id-relationship")
	public String getExternalIdRelationship() {
		return externalIdRelationship;
	}

    @JsonProperty("external-id-relationship")
	public void setExternalIdRelationship(String externalIdRelationship) {
		this.externalIdRelationship = externalIdRelationship;
	}

}
