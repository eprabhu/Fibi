package com.polus.fibicomp.manpowerintegration.dto.supervisoryorganization;

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
    "SUP_ORG_ID"
})
public class SupOrgReportEntry {

	@JsonProperty("SUP_ORG_ID")
    private String sUPORGID;

    @JsonIgnore
    private Map<String, Object> additionalProperties = new HashMap<String, Object>();

    @JsonProperty("SUP_ORG_ID")
	public String getsUPORGID() {
		return sUPORGID;
	}

    @JsonProperty("SUP_ORG_ID")
	public void setsUPORGID(String sUPORGID) {
		this.sUPORGID = sUPORGID;
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
