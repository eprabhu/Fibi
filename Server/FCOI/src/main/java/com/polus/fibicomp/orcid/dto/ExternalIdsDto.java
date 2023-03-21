package com.polus.fibicomp.orcid.dto;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({
    "external-id"
})
public class ExternalIdsDto {

	@JsonProperty("external-id")
    private List<ExternalIdDto> externalId = null;

	@JsonProperty("external-id")
	public List<ExternalIdDto> getExternalId() {
		return externalId;
	}

	@JsonProperty("external-id")
	public void setExternalId(List<ExternalIdDto> externalId) {
		this.externalId = externalId;
	}

}
