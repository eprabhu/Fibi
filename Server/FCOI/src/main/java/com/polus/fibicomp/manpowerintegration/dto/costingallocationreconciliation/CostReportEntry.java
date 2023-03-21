
package com.polus.fibicomp.manpowerintegration.dto.costingallocationreconciliation;

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
    "Allocation_Details_group",
    "EMPLOYEE_ID",
    "POSITION_ID"
})
public class CostReportEntry {

    @JsonProperty("Allocation_Details_group")
    private List<CostAllocationDetailsGroup> allocationDetailsGroup = null;
    @JsonProperty("EMPLOYEE_ID")
    private String eMPLOYEEID;
    @JsonProperty("POSITION_ID")
    private String pOSITIONID;
    @JsonIgnore
    private Map<String, Object> additionalProperties = new HashMap<String, Object>();

    @JsonProperty("Allocation_Details_group")
    public List<CostAllocationDetailsGroup> getAllocationDetailsGroup() {
        return allocationDetailsGroup;
    }

    @JsonProperty("Allocation_Details_group")
    public void setAllocationDetailsGroup(List<CostAllocationDetailsGroup> allocationDetailsGroup) {
        this.allocationDetailsGroup = allocationDetailsGroup;
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

    @JsonAnyGetter
    public Map<String, Object> getAdditionalProperties() {
        return this.additionalProperties;
    }

    @JsonAnySetter
    public void setAdditionalProperty(String name, Object value) {
        this.additionalProperties.put(name, value);
    }

}
