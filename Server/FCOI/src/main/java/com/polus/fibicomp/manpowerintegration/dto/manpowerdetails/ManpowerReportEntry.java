
package com.polus.fibicomp.manpowerintegration.dto.manpowerdetails;

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
    "JOB_CODE",
    "EMPLOYEE_ID",
    "POSITION_ID",
    "HIRE_DATE",
    "GENDER",
    "CONTRACT_END_DATE"
})
public class ManpowerReportEntry {

    @JsonProperty("JOB_CODE")
    private String jOBCODE;
    @JsonProperty("EMPLOYEE_ID")
    private String eMPLOYEEID;
    @JsonProperty("POSITION_ID")
    private String pOSITIONID;
    @JsonProperty("HIRE_DATE")
    private String hIREDATE;
    @JsonProperty("GENDER")
    private String gENDER;
    @JsonProperty("CONTRACT_END_DATE")
    private String cONTRACTENDDATE;
    @JsonIgnore
    private Map<String, Object> additionalProperties = new HashMap<String, Object>();

    @JsonProperty("JOB_CODE")
    public String getJOBCODE() {
        return jOBCODE;
    }

    @JsonProperty("JOB_CODE")
    public void setJOBCODE(String jOBCODE) {
        this.jOBCODE = jOBCODE;
    }

    @JsonProperty("EMPLOYEE_ID")
    public String getEMPLOYEEID() {
        return eMPLOYEEID;
    }

    @JsonProperty("EMPLOYEE_ID")
    public void setEMPLOYEEID(String eMPLOYEEID) {
        this.eMPLOYEEID = eMPLOYEEID;
    }

    @JsonProperty("POSITION_ID")
    public String getPOSITIONID() {
        return pOSITIONID;
    }

    @JsonProperty("POSITION_ID")
    public void setPOSITIONID(String pOSITIONID) {
        this.pOSITIONID = pOSITIONID;
    }

    @JsonProperty("HIRE_DATE")
    public String getHIREDATE() {
        return hIREDATE;
    }

    @JsonProperty("HIRE_DATE")
    public void setHIREDATE(String hIREDATE) {
        this.hIREDATE = hIREDATE;
    }

    @JsonProperty("GENDER")
    public String getGENDER() {
        return gENDER;
    }

    @JsonProperty("GENDER")
    public void setGENDER(String gENDER) {
        this.gENDER = gENDER;
    }

    @JsonProperty("CONTRACT_END_DATE")
    public String getCONTRACTENDDATE() {
        return cONTRACTENDDATE;
    }

    @JsonProperty("CONTRACT_END_DATE")
    public void setCONTRACTENDDATE(String cONTRACTENDDATE) {
        this.cONTRACTENDDATE = cONTRACTENDDATE;
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
