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
@JsonPropertyOrder({ "rfc:ZFI_GET_PROFIT_CENTER.Response" })
public class ProfitCenter {

	@JsonProperty("rfc:ZFI_GET_PROFIT_CENTER.Response")
	private ProfitCenterResponse profitCenterResponse;
	
	@JsonIgnore
	private Map<String, Object> additionalProperties = new HashMap<String, Object>();

	@JsonProperty("rfc:ZFI_GET_PROFIT_CENTER.Response")
	public ProfitCenterResponse getProfitCenterResponse() {
		return profitCenterResponse;
	}

	@JsonProperty("rfc:ZFI_GET_PROFIT_CENTER.Response")
	public void setProfitCenterResponse(ProfitCenterResponse profitCenterResponse) {
		this.profitCenterResponse = profitCenterResponse;
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
