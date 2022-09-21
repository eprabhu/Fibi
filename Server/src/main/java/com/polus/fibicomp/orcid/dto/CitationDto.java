package com.polus.fibicomp.orcid.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({
    "citation-type",
    "citation-value"
})
public class CitationDto {

	@JsonProperty("citation-type")
    private String citationType;

    @JsonProperty("citation-value")
    private String citationValue;

    @JsonProperty("citation-type")
    public String getCitationType() {
        return citationType;
    }

    @JsonProperty("citation-type")
    public void setCitationType(String citationType) {
        this.citationType = citationType;
    }

    @JsonProperty("citation-value")
    public String getCitationValue() {
        return citationValue;
    }

    @JsonProperty("citation-value")
    public void setCitationValue(String citationValue) {
        this.citationValue = citationValue;
    }

}
