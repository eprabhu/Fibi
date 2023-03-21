package com.polus.fibicomp.vo;

public class OrganizationSearchResult {

	private String organizationId;
	private String organizationName;
	
	public OrganizationSearchResult() {
	}

	public OrganizationSearchResult(String organizationId, String organizationName) {
		this.organizationId = organizationId;
		this.organizationName = organizationName;
	}

	public String getOrganizationId() {
		return organizationId;
	}

	public void setOrganizationId(String organizationId) {
		this.organizationId = organizationId;
	}

	public String getOrganizationName() {
		return organizationName;
	}

	public void setOrganizationName(String organizationName) {
		this.organizationName = organizationName;
	}
	
}
