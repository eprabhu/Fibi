package com.polus.fibicomp.manpowerintegration.dto.jobprofilechanges;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonAnyGetter;
import com.fasterxml.jackson.annotation.JsonAnySetter;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({
    "Report_Entry"
})
public class JobProfileChange {

	@JsonProperty("Report_Entry")
    private List<JobProfileChangeReportEntry> jobProfileChangeReportEntry = null;
    @JsonIgnore
    private Map<String, Object> additionalProperties = new HashMap<String, Object>();

    @JsonProperty("Report_Entry")
	public List<JobProfileChangeReportEntry> getJobProfileChangeReportEntry() {
		return jobProfileChangeReportEntry;
	}

	@JsonProperty("Report_Entry")
	public void setJobProfileChangeReportEntry(List<JobProfileChangeReportEntry> jobProfileChangeReportEntry) {
		this.jobProfileChangeReportEntry = jobProfileChangeReportEntry;
	}

	@JsonAnyGetter
    public Map<String, Object> getAdditionalProperties() {
        return this.additionalProperties;
    }

    @JsonAnySetter
    public void setAdditionalProperty(String name, Object value) {
        this.additionalProperties.put(name, value);
    }

}
