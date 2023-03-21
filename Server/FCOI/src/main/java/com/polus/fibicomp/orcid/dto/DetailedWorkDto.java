package com.polus.fibicomp.orcid.dto;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({
    "bulk"
})
public class DetailedWorkDto {

	@JsonProperty("bulk")
    private List<BulkDto> bulks = null;

	@JsonProperty("bulk")
	public List<BulkDto> getBulks() {
		return bulks;
	}

	@JsonProperty("bulk")
	public void setBulks(List<BulkDto> bulks) {
		this.bulks = bulks;
	}

}
