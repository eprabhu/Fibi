package com.polus.fibicomp.orcid.dto;

import com.polus.fibicomp.award.awardprojectoutcome.dto.ModuleDetails;

public class OrcidWorkLinkedAwardDto {

	private Integer awardPersonOrcidWorkId;

	private String personId;

	private Integer personOrcidWorkId;

	private String awardNumber;

	private ModuleDetails awardDetail;

	public Integer getAwardPersonOrcidWorkId() {
		return awardPersonOrcidWorkId;
	}

	public void setAwardPersonOrcidWorkId(Integer awardPersonOrcidWorkId) {
		this.awardPersonOrcidWorkId = awardPersonOrcidWorkId;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public Integer getPersonOrcidWorkId() {
		return personOrcidWorkId;
	}

	public void setPersonOrcidWorkId(Integer personOrcidWorkId) {
		this.personOrcidWorkId = personOrcidWorkId;
	}

	public String getAwardNumber() {
		return awardNumber;
	}

	public void setAwardNumber(String awardNumber) {
		this.awardNumber = awardNumber;
	}

	public ModuleDetails getAwardDetail() {
		return awardDetail;
	}

	public void setAwardDetail(ModuleDetails awardDetail) {
		this.awardDetail = awardDetail;
	}

}
