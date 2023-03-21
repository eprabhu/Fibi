package com.polus.fibicomp.manpowerintegration.vo;

public class AwardPersonVo {
	
	private Integer award_Id;

	private String personaId;

	private String fullName;

	private String leadUnitNumber;

	private String awardNumber;

	private String leadUnitName;

	public AwardPersonVo(Integer award_Id, String personaId, String fullName, String leadUnitNumber, String awardNumber, String leadUnitName) {
		super();
		this.award_Id = award_Id;
		this.personaId = personaId;
		this.fullName = fullName;
		this.leadUnitNumber = leadUnitNumber;
		this.awardNumber = awardNumber;
		this.leadUnitName = leadUnitName;
	}

	public Integer getAward_Id() {
		return award_Id;
	}

	public void setAward_Id(Integer award_Id) {
		this.award_Id = award_Id;
	}

	public String getPersonaId() {
		return personaId;
	}

	public void setPersonaId(String personaId) {
		this.personaId = personaId;
	}

	public String getFullName() {
		return fullName;
	}

	public void setFullName(String fullName) {
		this.fullName = fullName;
	}

	public String getLeadUnitNumber() {
		return leadUnitNumber;
	}

	public void setLeadUnitNumber(String leadUnitNumber) {
		this.leadUnitNumber = leadUnitNumber;
	}

	public String getAwardNumber() {
		return awardNumber;
	}

	public void setAwardNumber(String awardNumber) {
		this.awardNumber = awardNumber;
	}

	public String getLeadUnitName() {
		return leadUnitName;
	}

	public void setLeadUnitName(String leadUnitName) {
		this.leadUnitName = leadUnitName;
	}

}
