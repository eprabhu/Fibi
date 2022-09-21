package com.polus.fibicomp.orcid.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({
    "work"
})
public class BulkDto {

	@JsonProperty("work")
    private WorkDto work;

	@JsonProperty("work")
	public WorkDto getWork() {
		return work;
	}

	@JsonProperty("work")
	public void setWork(WorkDto work) {
		this.work = work;
	}

}
