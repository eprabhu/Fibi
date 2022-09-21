package com.polus.fibicomp.manpower.dto;

public class ManpowerPersonSearchResult {

	private String personId;

	private String positionId;

	private String userName;

	private String fullName;

	private String positionOwnedByAward;

	private String jobProfileTypeCode;

	private String jobProfileType;

	public ManpowerPersonSearchResult(String personId, String fullName, String userName, String positionId) {
		super();
		this.personId = personId;
		this.fullName = fullName;
		this.positionId = positionId;
		this.userName =  userName;
	}

	public ManpowerPersonSearchResult(String personId, String fullName, String userName) {
		super();
		this.personId = personId;
		this.fullName = fullName;
		this.userName = userName;
	}

	public ManpowerPersonSearchResult(String personId, String fullName, String userName, String positionId, String jobProfileTypeCode, String jobProfileType) {
		super();
		this.personId = personId;
		this.fullName = fullName;
		this.positionId = positionId;
		this.userName =  userName;
		this.jobProfileTypeCode = jobProfileTypeCode;
		this.jobProfileType = jobProfileType;
	}

	public ManpowerPersonSearchResult() {

	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public String getFullName() {
		return fullName;
	}

	public void setFullName(String fullName) {
		this.fullName = fullName;
	}

	public String getPositionId() {
		return positionId;
	}

	public void setPositionId(String positionId) {
		this.positionId = positionId;
	}

	public String getUserName() {
		return userName;
	}

	public void setUserName(String userName) {
		this.userName = userName;
	}

	public String getPositionOwnedByAward() {
		return positionOwnedByAward;
	}

	public void setPositionOwnedByAward(String positionOwnedByAward) {
		this.positionOwnedByAward = positionOwnedByAward;
	}

	public String getJobProfileTypeCode() {
		return jobProfileTypeCode;
	}

	public void setJobProfileTypeCode(String jobProfileTypeCode) {
		this.jobProfileTypeCode = jobProfileTypeCode;
	}

	public String getJobProfileType() {
		return jobProfileType;
	}

	public void setJobProfileType(String jobProfileType) {
		this.jobProfileType = jobProfileType;
	}

}
