package com.polus.fibicomp.orcid.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({
    "source-orcid",
    "source-client-id",
    "source-name",
    "assertion-origin-orcid",
    "assertion-origin-client-id",
    "assertion-origin-name"
})
public class SourceDto {

	@JsonProperty("source-orcid")
	private SourceOrcidDto sourceOrcid;

	@JsonProperty("source-client-id")
	private SourceClientIdDto sourceClientId;

	@JsonProperty("source-name")
	private SourceNameDto sourceName;

	@JsonProperty("assertion-origin-orcid")
	private AssertionOriginOrcidDto assertionOriginOrcid;

	@JsonProperty("assertion-origin-client-id")
	private AssertionOriginClientIdDto assertionOriginClientId;

	@JsonProperty("assertion-origin-name")
	private AssertionOriginNameDto assertionOriginName;

	@JsonProperty("source-orcid")
	public SourceOrcidDto getSourceOrcid() {
		return sourceOrcid;
	}

	@JsonProperty("source-orcid")
	public void setSourceOrcid(SourceOrcidDto sourceOrcid) {
		this.sourceOrcid = sourceOrcid;
	}

	@JsonProperty("source-client-id")
	public SourceClientIdDto getSourceClientId() {
		return sourceClientId;
	}

	@JsonProperty("source-client-id")
	public void setSourceClientId(SourceClientIdDto sourceClientId) {
		this.sourceClientId = sourceClientId;
	}

	@JsonProperty("source-name")
	public SourceNameDto getSourceName() {
		return sourceName;
	}

	@JsonProperty("source-name")
	public void setSourceName(SourceNameDto sourceName) {
		this.sourceName = sourceName;
	}

	@JsonProperty("assertion-origin-orcid")
	public AssertionOriginOrcidDto getAssertionOriginOrcid() {
		return assertionOriginOrcid;
	}

	@JsonProperty("assertion-origin-orcid")
	public void setAssertionOriginOrcid(AssertionOriginOrcidDto assertionOriginOrcid) {
		this.assertionOriginOrcid = assertionOriginOrcid;
	}

	@JsonProperty("assertion-origin-client-id")
	public AssertionOriginClientIdDto getAssertionOriginClientId() {
		return assertionOriginClientId;
	}

	@JsonProperty("assertion-origin-client-id")
	public void setAssertionOriginClientId(AssertionOriginClientIdDto assertionOriginClientId) {
		this.assertionOriginClientId = assertionOriginClientId;
	}

	@JsonProperty("assertion-origin-name")
	public AssertionOriginNameDto getAssertionOriginName() {
		return assertionOriginName;
	}

	@JsonProperty("assertion-origin-name")
	public void setAssertionOriginName(AssertionOriginNameDto assertionOriginName) {
		this.assertionOriginName = assertionOriginName;
	}

}
