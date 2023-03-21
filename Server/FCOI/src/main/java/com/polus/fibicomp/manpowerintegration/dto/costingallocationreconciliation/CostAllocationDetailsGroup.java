
package com.polus.fibicomp.manpowerintegration.dto.costingallocationreconciliation;

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
    "COST_ALLOC_PROJECT_ID",
    "COST_ALLOC_END_DATE",
    "COST_ALLOC_START_DATE",
    "COST_ALLOC_PERCENT"
})
public class CostAllocationDetailsGroup {

    @JsonProperty("COST_ALLOC_PROJECT_ID")
    private String cOSTALLOCPROJECTID;
    @JsonProperty("COST_ALLOC_END_DATE")
    private String cOSTALLOCENDDATE;
    @JsonProperty("COST_ALLOC_START_DATE")
    private String cOSTALLOCSTARTDATE;
    @JsonProperty("COST_ALLOC_PERCENT")
    private String cOSTALLOCPERCENT;
    @JsonIgnore
    private Map<String, Object> additionalProperties = new HashMap<String, Object>();

    @JsonProperty("COST_ALLOC_PROJECT_ID")
    public String getCOSTALLOCPROJECTID() {
        return cOSTALLOCPROJECTID;
    }

    @JsonProperty("COST_ALLOC_PROJECT_ID")
    public void setCOSTALLOCPROJECTID(String cOSTALLOCPROJECTID) {
        this.cOSTALLOCPROJECTID = cOSTALLOCPROJECTID;
    }

    @JsonProperty("COST_ALLOC_END_DATE")
    public String getCOSTALLOCENDDATE() {
        return cOSTALLOCENDDATE;
    }

    @JsonProperty("COST_ALLOC_END_DATE")
    public void setCOSTALLOCENDDATE(String cOSTALLOCENDDATE) {
        this.cOSTALLOCENDDATE = cOSTALLOCENDDATE;
    }

    @JsonProperty("COST_ALLOC_START_DATE")
    public String getCOSTALLOCSTARTDATE() {
        return cOSTALLOCSTARTDATE;
    }

    @JsonProperty("COST_ALLOC_START_DATE")
    public void setCOSTALLOCSTARTDATE(String cOSTALLOCSTARTDATE) {
        this.cOSTALLOCSTARTDATE = cOSTALLOCSTARTDATE;
    }

    @JsonProperty("COST_ALLOC_PERCENT")
    public String getCOSTALLOCPERCENT() {
        return cOSTALLOCPERCENT;
    }

    @JsonProperty("COST_ALLOC_PERCENT")
    public void setCOSTALLOCPERCENT(String cOSTALLOCPERCENT) {
        this.cOSTALLOCPERCENT = cOSTALLOCPERCENT;
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
