package com.polus.fibicomp.award.awardprojectoutcome.dto;

import java.util.ArrayList;
import java.util.List;

public class AwardOutcomeDTO {

	private String searchString;

	private Integer awardId;

	private String awardNumber;

	private Integer moduleCode;

	private List<ModuleDetails> moduleDetails;

	public AwardOutcomeDTO() {
		moduleDetails = new ArrayList<>();
	}

	public String getSearchString() {
		return searchString;
	}

	public void setSearchString(String searchString) {
		this.searchString = searchString;
	}

	public Integer getAwardId() {
		return awardId;
	}

	public void setAwardId(Integer awardId) {
		this.awardId = awardId;
	}

	public Integer getModuleCode() {
		return moduleCode;
	}

	public void setModuleCode(Integer moduleCode) {
		this.moduleCode = moduleCode;
	}

	public List<ModuleDetails> getModuleDetails() {
		return moduleDetails;
	}

	public void setModuleDetails(List<ModuleDetails> moduleDetails) {
		this.moduleDetails = moduleDetails;
	}

	public String getAwardNumber() {
		return awardNumber;
	}

	public void setAwardNumber(String awardNumber) {
		this.awardNumber = awardNumber;
	}

}
