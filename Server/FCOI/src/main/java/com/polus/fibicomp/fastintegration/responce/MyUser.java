package com.polus.fibicomp.fastintegration.responce;

public class MyUser {

	private String batchId;
	private String classDescription;
	private String sponsorClass;
	private String classType;
	private String createStatus;
	
	public MyUser(String sponsorClass, String classDescription, String classType, String batchId, String createStatus) {
		this.sponsorClass = sponsorClass;
		this.classDescription = classDescription;
		this.classType = classType;
		this.batchId = batchId;
		this.createStatus = createStatus;
	}

	public String getBatchId() {
		return batchId;
	}

	public void setBatchId(String batchId) {
		this.batchId = batchId;
	}

	public String getClassDescription() {
		return classDescription;
	}

	public void setClassDescription(String classDescription) {
		this.classDescription = classDescription;
	}

	public String getSponsorClass() {
		return sponsorClass;
	}

	public void setSponsorClass(String sponsorClass) {
		this.sponsorClass = sponsorClass;
	}

	public String getClassType() {
		return classType;
	}

	public void setClassType(String classType) {
		this.classType = classType;
	}

	public String getCreateStatus() {
		return createStatus;
	}

	public void setCreateStatus(String createStatus) {
		this.createStatus = createStatus;
	}
	
	
}
