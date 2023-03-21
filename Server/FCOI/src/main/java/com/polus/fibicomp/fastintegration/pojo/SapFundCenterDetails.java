package com.polus.fibicomp.fastintegration.pojo;

import java.util.HashMap;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonAnyGetter;
import com.fasterxml.jackson.annotation.JsonAnySetter;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({ "COMPANY_CODE", "FUND_CENTRE", "FUND_CENTRE_DESCRIPTION", "FUNCTIONAL_AREA", "VALID_FROM",
		"VALID_TO" })
public class SapFundCenterDetails {

	@JsonProperty("COMPANY_CODE")
	private String companyCode;

	@JsonProperty("FUND_CENTRE")
	private String fundCentre;

	@JsonProperty("FUND_CENTRE_DESCRIPTION")
	private String fundCentreDescription;
	
	@JsonProperty("FUND_CENTRE_L_DESCRIPTION")
	private String fundCentreLongDescription;

	@JsonProperty("FUNCTIONAL_AREA")
	private String functionalArea;

	@JsonProperty("VALID_FROM")
	private String validFrom;

	@JsonProperty("VALID_TO")
	private String validTo;

	public String getFundCentreLongDescription() {
		return fundCentreLongDescription;
	}

	public void setFundCentreLongDescription(String fundCentreLongDescription) {
		this.fundCentreLongDescription = fundCentreLongDescription;
	}

	@JsonIgnore
	private Map<String, Object> additionalProperties = new HashMap<String, Object>();

	@JsonProperty("COMPANY_CODE")
	public String getCompanyCode() {
		return companyCode;
	}

	@JsonProperty("COMPANY_CODE")
	public void setCompanyCode(String companyCode) {
		this.companyCode = companyCode;
	}

	@JsonProperty("FUND_CENTRE")
	public String getFundCentre() {
		return fundCentre;
	}

	@JsonProperty("FUND_CENTRE")
	public void setFundCentre(String fundCentre) {
		this.fundCentre = fundCentre;
	}

	@JsonProperty("FUND_CENTRE_DESCRIPTION")
	public String getFundCentreDescription() {
		return fundCentreDescription;
	}

	@JsonProperty("FUND_CENTRE_DESCRIPTION")
	public void setFundCentreDescription(String fundCentreDescription) {
		this.fundCentreDescription = fundCentreDescription;
	}

	@JsonProperty("FUNCTIONAL_AREA")
	public String getFunctionalArea() {
		return functionalArea;
	}

	@JsonProperty("FUNCTIONAL_AREA")
	public void setFunctionalArea(String functionalArea) {
		this.functionalArea = functionalArea;
	}

	@JsonProperty("VALID_FROM")
	public String getValidFrom() {
		return validFrom;
	}

	@JsonProperty("VALID_FROM")
	public void setValidFrom(String validFrom) {
		this.validFrom = validFrom;
	}

	@JsonProperty("VALID_TO")
	public String getValidTo() {
		return validTo;
	}

	@JsonProperty("VALID_TO")
	public void setValidTo(String validTo) {
		this.validTo = validTo;
	}

	@JsonAnyGetter
	public Map<String, Object> getAdditionalProperties() {
		return this.additionalProperties;
	}

	@JsonAnySetter
	public void setAdditionalProperty(String name, Object value) {
		this.additionalProperties.put(name, value);
	}
}
