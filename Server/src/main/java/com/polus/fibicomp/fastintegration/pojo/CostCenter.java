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
@JsonPropertyOrder({ "rfc:ZFI_GET_COST_CENTER.Response" })
public class CostCenter {

	@JsonProperty("rfc:ZFI_GET_COST_CENTER.Response")
	private CostCenterResponse costCenterResponse;
	
	@JsonIgnore
	private Map<String, Object> additionalProperties = new HashMap<String, Object>();

	@JsonProperty("rfc:ZFI_GET_COST_CENTER.Response")
	public CostCenterResponse getCostCenterResponse() {
		return costCenterResponse;
	}

	@JsonProperty("rfc:ZFI_GET_COST_CENTER.Response")
	public void setCostCenterResponse(CostCenterResponse costCenterResponse) {
		this.costCenterResponse = costCenterResponse;
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
