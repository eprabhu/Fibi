package com.polus.fibicomp.orcid.dto;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({
    "contributor"
})
public class ContributorsDto {

	@JsonProperty("contributor")
    private List<ContributorDto> contributor = null;

	@JsonProperty("contributor")
	public List<ContributorDto> getContributor() {
		return contributor;
	}

	@JsonProperty("contributor")
	public void setContributor(List<ContributorDto> contributor) {
		this.contributor = contributor;
	}

}
