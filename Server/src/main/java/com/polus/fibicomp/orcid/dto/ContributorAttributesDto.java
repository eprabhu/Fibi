package com.polus.fibicomp.orcid.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({
    "contributor-sequence",
    "contributor-role"
})
public class ContributorAttributesDto {

	@JsonProperty("contributor-sequence")
	private String contributorSequence;

    @JsonProperty("contributor-role")
    private String contributorRole;

    @JsonProperty("contributor-sequence")
    public String getContributorSequence() {
        return contributorSequence;
    }

    @JsonProperty("contributor-sequence")
    public void setContributorSequence(String contributorSequence) {
        this.contributorSequence = contributorSequence;
    }

    @JsonProperty("contributor-role")
    public String getContributorRole() {
        return contributorRole;
    }

    @JsonProperty("contributor-role")
    public void setContributorRole(String contributorRole) {
        this.contributorRole = contributorRole;
    }

}
