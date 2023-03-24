package com.polus.fibicomp.manpowerintegration.dto.hrbusinesspartner;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({
"Report_Entry"
})
public class WorkdayHRBusinessPartner {

	@JsonProperty("Report_Entry")
	private List<HRBPReportEntry> reportEntries = null;

	@JsonProperty("Report_Entry")
	public List<HRBPReportEntry> getReportEntries() {
		return reportEntries;
	}

	@JsonProperty("Report_Entry")
	public void setReportEntries(List<HRBPReportEntry> reportEntries) {
		this.reportEntries = reportEntries;
	}

}
