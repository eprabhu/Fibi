package com.polus.fibicomp.manpowerintegration.dto.jobprofile;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({
    "Report_Entry"
})
public class WorkdayJobProfile {

	@JsonProperty("Report_Entry")
	private List<JobProfileReportEntry> jobProfileReportEntries = null;

	@JsonProperty("Report_Entry")
	public List<JobProfileReportEntry> getJobProfileReportEntries() {
		return jobProfileReportEntries;
	}

	@JsonProperty("Report_Entry")
	public void setJobProfileReportEntries(List<JobProfileReportEntry> jobProfileReportEntries) {
		this.jobProfileReportEntries = jobProfileReportEntries;
	}

}
