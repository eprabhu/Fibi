
package com.polus.fibicomp.manpowerintegration.dto.citizenship;

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
    "NATIONALITY",
    "EMPLOYEE_ID",
    "POSITION_ID",
    "CITIZENSHIP"
})
public class CitizenReportEntry {

    @JsonProperty("NATIONALITY")
    private String nATIONALITY;
    @JsonProperty("EMPLOYEE_ID")
    private String eMPLOYEEID;
    @JsonProperty("POSITION_ID")
    private String pOSITIONID;
    @JsonProperty("CITIZENSHIP")
    private String cITIZENSHIP;
    @JsonIgnore
    private Map<String, Object> additionalProperties = new HashMap<String, Object>();

    @JsonProperty("NATIONALITY")
    public String getNATIONALITY() {
        return nATIONALITY;
    }

    @JsonProperty("NATIONALITY")
    public void setNATIONALITY(String nATIONALITY) {
        this.nATIONALITY = nATIONALITY;
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

    @JsonProperty("CITIZENSHIP")
    public String getCITIZENSHIP() {
        return cITIZENSHIP;
    }

    @JsonProperty("CITIZENSHIP")
    public void setCITIZENSHIP(String cITIZENSHIP) {
        this.cITIZENSHIP = cITIZENSHIP;
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
