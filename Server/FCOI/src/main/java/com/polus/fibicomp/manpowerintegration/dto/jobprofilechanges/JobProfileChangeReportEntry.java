package com.polus.fibicomp.manpowerintegration.dto.jobprofilechanges;

import java.util.HashMap;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonAnyGetter;
import com.fasterxml.jackson.annotation.JsonAnySetter;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({
    "JOB_CODE_CURRENT",
    "EFFECTIVE_DATE",
    "EMPLOYEE_ID",
    "JOB_CODE_PROPOSED"
})
public class JobProfileChangeReportEntry {

	@JsonProperty("JOB_CODE_CURRENT")
    private String jOBCODECURRENT;
    @JsonProperty("EFFECTIVE_DATE")
    private String eFFECTIVEDATE;
    @JsonProperty("EMPLOYEE_ID")
    private String eMPLOYEEID;
    @JsonProperty("JOB_CODE_PROPOSED")
    private String jOBCODEPROPOSED;

    @JsonIgnore
    private Map<String, Object> additionalProperties = new HashMap<String, Object>();

    @JsonProperty("JOB_CODE_CURRENT")
	public String getjOBCODECURRENT() {
		return jOBCODECURRENT;
	}

    @JsonProperty("JOB_CODE_CURRENT")
	public void setjOBCODECURRENT(String jOBCODECURRENT) {
		this.jOBCODECURRENT = jOBCODECURRENT;
	}

    @JsonProperty("EFFECTIVE_DATE")
	public String geteFFECTIVEDATE() {
		return eFFECTIVEDATE;
	}

    @JsonProperty("EFFECTIVE_DATE")
	public void seteFFECTIVEDATE(String eFFECTIVEDATE) {
		this.eFFECTIVEDATE = eFFECTIVEDATE;
	}

    @JsonProperty("EMPLOYEE_ID")
	public String geteMPLOYEEID() {
		return eMPLOYEEID;
	}

    @JsonProperty("EMPLOYEE_ID")
	public void seteMPLOYEEID(String eMPLOYEEID) {
		this.eMPLOYEEID = eMPLOYEEID;
	}

    @JsonProperty("JOB_CODE_PROPOSED")
	public String getjOBCODEPROPOSED() {
		return jOBCODEPROPOSED;
	}

    @JsonProperty("JOB_CODE_PROPOSED")
	public void setjOBCODEPROPOSED(String jOBCODEPROPOSED) {
		this.jOBCODEPROPOSED = jOBCODEPROPOSED;
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
