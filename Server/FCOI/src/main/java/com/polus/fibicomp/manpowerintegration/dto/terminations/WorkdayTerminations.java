package com.polus.fibicomp.manpowerintegration.dto.terminations;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({
    "Report_Entry"
})
public class WorkdayTerminations {

	@JsonProperty("Report_Entry")
    private List<TerminationsReportEntry> terminationReportEntries = null;

	@JsonProperty("Report_Entry")
	public List<TerminationsReportEntry> getTerminationReportEntries() {
		return terminationReportEntries;
	}

	@JsonProperty("Report_Entry")
	public void setTerminationReportEntries(List<TerminationsReportEntry> terminationReportEntries) {
		this.terminationReportEntries = terminationReportEntries;
	}

}
