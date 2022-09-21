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
@JsonPropertyOrder({ "EX_FC_MASTER" })
public class FundCenterResponse {

	@JsonProperty("EX_FC_MASTER")
	private FundCenterMasterResponse fundCenterMasterResponse;

	@JsonIgnore
	private Map<String, Object> additionalProperties = new HashMap<String, Object>();

	@JsonProperty("EX_FC_MASTER")
	public FundCenterMasterResponse getFundCenterMasterResponse() {
		return fundCenterMasterResponse;
	}

	@JsonProperty("EX_FC_MASTER")
	public void setFundCenterMasterResponse(FundCenterMasterResponse fundCenterMasterResponse) {
		this.fundCenterMasterResponse = fundCenterMasterResponse;
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
