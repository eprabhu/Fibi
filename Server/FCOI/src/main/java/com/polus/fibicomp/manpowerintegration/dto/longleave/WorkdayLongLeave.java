package com.polus.fibicomp.manpowerintegration.dto.longleave;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({
    "Report_Entry"
})
public class WorkdayLongLeave {

	@JsonProperty("Report_Entry")
    private List<LongLeaveReportEntry> longLeaveReportEntries = null;

	@JsonProperty("Report_Entry")
	public List<LongLeaveReportEntry> getLongLeaveReportEntries() {
		return longLeaveReportEntries;
	}

	@JsonProperty("Report_Entry")
	public void setLongLeaveReportEntries(List<LongLeaveReportEntry> longLeaveReportEntries) {
		this.longLeaveReportEntries = longLeaveReportEntries;
	}

}
