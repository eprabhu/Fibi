package com.polus.fibicomp.orcid.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({
    "contributor-orcid",
    "credit-name",
    "contributor-email",
    "contributor-attributes"
})
public class ContributorDto {

	@JsonProperty("contributor-orcid")
	private ContributorOrcidDto contributorOrcid;

	@JsonProperty("credit-name")
	private CreditNameDto creditName;

	@JsonProperty("contributor-email")
	private ContributorEmailDto contributorEmail;

	@JsonProperty("contributor-attributes")
	private ContributorAttributesDto contributorAttributes;

	@JsonProperty("contributor-orcid")
	public ContributorOrcidDto getContributorOrcid() {
		return contributorOrcid;
	}

	@JsonProperty("contributor-orcid")
	public void setContributorOrcid(ContributorOrcidDto contributorOrcid) {
		this.contributorOrcid = contributorOrcid;
	}

	@JsonProperty("credit-name")
	public CreditNameDto getCreditName() {
		return creditName;
	}

	@JsonProperty("credit-name")
	public void setCreditName(CreditNameDto creditName) {
		this.creditName = creditName;
	}

	@JsonProperty("contributor-email")
	public ContributorEmailDto getContributorEmail() {
		return contributorEmail;
	}

	@JsonProperty("contributor-email")
	public void setContributorEmail(ContributorEmailDto contributorEmail) {
		this.contributorEmail = contributorEmail;
	}

	@JsonProperty("contributor-attributes")
	public ContributorAttributesDto getContributorAttributes() {
		return contributorAttributes;
	}

	@JsonProperty("contributor-attributes")
	public void setContributorAttributes(ContributorAttributesDto contributorAttributes) {
		this.contributorAttributes = contributorAttributes;
	}

}
