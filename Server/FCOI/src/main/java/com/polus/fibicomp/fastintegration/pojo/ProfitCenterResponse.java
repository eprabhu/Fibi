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
@JsonPropertyOrder({ "EX_PC_MASTER" })
public class ProfitCenterResponse {

	@JsonProperty("EX_PC_MASTER")
	private ProfitCenterMasterResponse profitCenterMasterResponse;
	
	@JsonIgnore
	private Map<String, Object> additionalProperties = new HashMap<String, Object>();

	@JsonProperty("EX_PC_MASTER")
	public ProfitCenterMasterResponse getProfitCenterMasterResponse() {
		return profitCenterMasterResponse;
	}

	@JsonProperty("EX_PC_MASTER")
	public void setProfitCenterMasterResponse(ProfitCenterMasterResponse profitCenterMasterResponse) {
		this.profitCenterMasterResponse = profitCenterMasterResponse;
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
