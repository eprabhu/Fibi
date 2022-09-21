package com.polus.fibicomp.award.vo;

public class MaintainAwardHierarchyVO {

	private String parentAwardId;
	private String parentAwardNumber;
	private String acType;
	private String rootAwardNumber;
	private String loggedUserName;
	private String awardNumber;
	private Integer awardId;
	private Boolean copyQuestionnaire = Boolean.FALSE;
	private Boolean copyOtherInformation = Boolean.FALSE;

	public String getParentAwardId() {
		return parentAwardId;
	}

	public void setParentAwardId(String parentAwardId) {
		this.parentAwardId = parentAwardId;
	}

	public String getAcType() {
		return acType;
	}

	public void setAcType(String acType) {
		this.acType = acType;
	}

	public String getRootAwardNumber() {
		return rootAwardNumber;
	}

	public void setRootAwardNumber(String rootAwardNumber) {
		this.rootAwardNumber = rootAwardNumber;
	}

	public String getParentAwardNumber() {
		return parentAwardNumber;
	}

	public void setParentAwardNumber(String parentAwardNumber) {
		this.parentAwardNumber = parentAwardNumber;
	}

	public String getLoggedUserName() {
		return loggedUserName;
	}

	public void setLoggedUserName(String loggedUserName) {
		this.loggedUserName = loggedUserName;
	}

	public String getAwardNumber() {
		return awardNumber;
	}

	public void setAwardNumber(String awardNumber) {
		this.awardNumber = awardNumber;
	}

	public Integer getAwardId() {
		return awardId;
	}

	public void setAwardId(Integer awardId) {
		this.awardId = awardId;
	}

	public Boolean getCopyQuestionnaire() {
		return copyQuestionnaire;
	}

	public void setCopyQuestionnaire(Boolean copyQuestionnaire) {
		this.copyQuestionnaire = copyQuestionnaire;
	}

	public Boolean getCopyOtherInformation() {
		return copyOtherInformation;
	}

	public void setCopyOtherInformation(Boolean copyOtherInformation) {
		this.copyOtherInformation = copyOtherInformation;
	}

}
