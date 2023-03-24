package com.polus.fibicomp.search.pojo;

public class SearchResult {

	private Integer moduleCode;
	
	private String searchText;

	private Integer sponsorGroupId;

	public Integer getModuleCode() {
		return moduleCode;
	}

	public void setModuleCode(Integer moduleCode) {
		this.moduleCode = moduleCode;
	}

	public String getSearchText() {
		return searchText;
	}

	public void setSearchText(String searchText) {
		this.searchText = searchText;
	}

	public Integer getSponsorGroupId() {
		return sponsorGroupId;
	}

	public void setSponsorGroupId(Integer sponsorGroupId) {
		this.sponsorGroupId = sponsorGroupId;
	}
}
