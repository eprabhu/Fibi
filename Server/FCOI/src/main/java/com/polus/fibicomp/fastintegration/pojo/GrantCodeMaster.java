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
public class GrantCodeMaster {

	@JsonProperty("item")
	private List<GrantCodeDetails> grantCodeDetails = null;
	
	@JsonIgnore
	private Map<String, Object> additionalProperties = new HashMap<String, Object>();

	@JsonProperty("item")
	public List<GrantCodeDetails> getGrantCodeDetails() {
		return grantCodeDetails;
	}

	@JsonProperty("item")
	public void setGrantCodeDetails(List<GrantCodeDetails> grantCodeDetails) {
		this.grantCodeDetails = grantCodeDetails;
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
