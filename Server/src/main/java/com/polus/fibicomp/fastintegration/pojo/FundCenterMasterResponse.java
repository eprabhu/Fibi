package com.polus.fibicomp.fastintegration.pojo;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonAnyGetter;
import com.fasterxml.jackson.annotation.JsonAnySetter;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({ "item" })
public class FundCenterMasterResponse {

	@JsonProperty("item")
	private List<SapFundCenterDetails> sapFundCenterDetails = null;

	@JsonIgnore
	private Map<String, Object> additionalProperties = new HashMap<String, Object>();

	@JsonProperty("item")
	public List<SapFundCenterDetails> getSapFundCenterDetails() {
		return sapFundCenterDetails;
	}

	@JsonProperty("item")
	public void setSapCostCenterDetails(List<SapFundCenterDetails> sapFundCenterDetails) {
		this.sapFundCenterDetails = sapFundCenterDetails;
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
