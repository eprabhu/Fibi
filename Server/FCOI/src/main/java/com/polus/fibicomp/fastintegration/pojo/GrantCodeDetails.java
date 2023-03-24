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
@JsonPropertyOrder({ "GRANT_NUMBER", "COMPANY_CODE", "GM_SPONSOR", "SHORT_DESC", "DESCRIPTION", "GRANT_TYPE",
		"VALID_FROM", "VALID_TO" })
public class GrantCodeDetails {

	@JsonProperty("GRANT_NUMBER")
	private String grantNumber;

	@JsonProperty("COMPANY_CODE")
	private String companyCode;

	@JsonProperty("GM_SPONSOR")
	private String gmSponsor;

	@JsonProperty("SHORT_DESC")
	private String shortDesc;

	@JsonProperty("DESCRIPTION")
	private String description;

	@JsonProperty("GRANT_TYPE")
	private String grantType;

	@JsonProperty("VALID_FROM")
	private String validFrom;

	@JsonProperty("VALID_TO")
	private String validTo;

	@JsonIgnore
	private Map<String, Object> additionalProperties = new HashMap<String, Object>();

	@JsonProperty("GRANT_NUMBER")
	public String getGrantNumber() {
		return grantNumber;
	}

	@JsonProperty("GRANT_NUMBER")
	public void setGrantNumber(String grantNumber) {
		this.grantNumber = grantNumber;
	}

	@JsonProperty("COMPANY_CODE")
	public String getCompanyCode() {
		return companyCode;
	}

	@JsonProperty("COMPANY_CODE")
	public void setCompanyCode(String companyCode) {
		this.companyCode = companyCode;
	}

	@JsonProperty("GM_SPONSOR")
	public String getGmSponsor() {
		return gmSponsor;
	}

	@JsonProperty("GM_SPONSOR")
	public void setGmSponsor(String gmSponsor) {
		this.gmSponsor = gmSponsor;
	}

	@JsonProperty("SHORT_DESC")
	public String getShortDesc() {
		return shortDesc;
	}

	@JsonProperty("SHORT_DESC")
	public void setShortDesc(String shortDesc) {
		this.shortDesc = shortDesc;
	}

	@JsonProperty("DESCRIPTION")
	public String getDescription() {
		return description;
	}

	@JsonProperty("DESCRIPTION")
	public void setDescription(String description) {
		this.description = description;
	}

	@JsonProperty("GRANT_TYPE")
	public String getGrantType() {
		return grantType;
	}

	@JsonProperty("GRANT_TYPE")
	public void setGrantType(String grantType) {
		this.grantType = grantType;
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
