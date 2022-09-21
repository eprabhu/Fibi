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
@JsonPropertyOrder({ "EX_CC_MASTER" })
public class CostCenterResponse {

	@JsonProperty("EX_CC_MASTER")
	private CostCenterMasterResponse costCenterMasterResponse;
	
	@JsonIgnore
	private Map<String, Object> additionalProperties = new HashMap<String, Object>();

	@JsonProperty("EX_CC_MASTER")
	public CostCenterMasterResponse getCostCenterMasterResponse() {
		return costCenterMasterResponse;
	}

	@JsonProperty("EX_CC_MASTER")
	public void setCostCenterMasterResponse(CostCenterMasterResponse costCenterMasterResponse) {
		this.costCenterMasterResponse = costCenterMasterResponse;
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
