package com.polus.fibicomp.orcid.dto;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({
    "last-modified-date",
    "external-ids",
    "work-summary"
})
public class GroupDto {

	@JsonProperty("last-modified-date")
    private LastModifiedDateDto lastModifiedDate;

    @JsonProperty("external-ids")
    private ExternalIdsDto externalIds;

    @JsonProperty("work-summary")
    private List<WorkSummaryDto> workSummary = null;

    @JsonProperty("last-modified-date")
	public LastModifiedDateDto getLastModifiedDate() {
		return lastModifiedDate;
	}

	@JsonProperty("last-modified-date")
	public void setLastModifiedDate(LastModifiedDateDto lastModifiedDate) {
		this.lastModifiedDate = lastModifiedDate;
	}

	@JsonProperty("external-ids")
	public ExternalIdsDto getExternalIds() {
		return externalIds;
	}

	@JsonProperty("external-ids")
	public void setExternalIds(ExternalIdsDto externalIds) {
		this.externalIds = externalIds;
	}

	@JsonProperty("work-summary")
	public List<WorkSummaryDto> getWorkSummary() {
		return workSummary;
	}

	@JsonProperty("work-summary")
	public void setWorkSummary(List<WorkSummaryDto> workSummary) {
		this.workSummary = workSummary;
	}

}
