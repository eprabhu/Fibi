package com.polus.fibicomp.manpowerintegration.dto.terminations;

public class TerminationsResourceResultDto {

	private Integer awardId;

	private String fullName;

	private String personId;

	public TerminationsResourceResultDto(Integer awardId, String fullName, String personId) {
		super();
		this.awardId = awardId;
		this.fullName = fullName;
		this.personId = personId;
	}

	public Integer getAwardId() {
		return awardId;
	}

	public void setAwardId(Integer awardId) {
		this.awardId = awardId;
	}

	public String getFullName() {
		return fullName;
	}

	public void setFullName(String fullName) {
		this.fullName = fullName;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

}
