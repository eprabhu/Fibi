package com.polus.fibicomp.orcid.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({
    "value",
    "transient"
})
public class ExternalIdNormalizedDto {

	@JsonProperty("value")
    private String value;

    @JsonProperty("transient")
    private boolean transientFlag;

    @JsonProperty("value")
	public String getValue() {
		return value;
	}

    @JsonProperty("value")
	public void setValue(String value) {
		this.value = value;
	}

    @JsonProperty("transient")
	public boolean isTransientFlag() {
		return transientFlag;
	}

    @JsonProperty("transient")
	public void setTransientFlag(boolean transientFlag) {
		this.transientFlag = transientFlag;
	}

}
