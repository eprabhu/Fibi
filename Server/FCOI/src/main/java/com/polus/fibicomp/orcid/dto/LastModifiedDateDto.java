package com.polus.fibicomp.orcid.dto;

import java.sql.Timestamp;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({
    "value"
})
public class LastModifiedDateDto {

	@JsonProperty("value")
    private Timestamp value;

	@JsonProperty("value")
	public Timestamp getValue() {
		return value;
	}

	@JsonProperty("value")
	public void setValue(Timestamp value) {
		this.value = value;
	}

}
