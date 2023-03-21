package com.polus.fibicomp.award.vo;

import com.polus.fibicomp.award.pojo.AwardCostShare;

public class AwardCostShareVO {

	private Integer awardCostShareId;

	private AwardCostShare awardCostShare;

	private String updateType;

	private String message;

	private String personId;

	private Integer awardId;

	private String updateUser;

	public AwardCostShareVO() {
		this.awardCostShare = new AwardCostShare();
		this.awardCostShareId = null;
		this.updateType = "SAVE";
		this.message = "";
		this.personId = "10000000001";
	}

	public Integer getAwardCostShareId() {
		return awardCostShareId;
	}

	public void setAwardCostShareId(Integer awardCostShareId) {
		this.awardCostShareId = awardCostShareId;
	}

	public AwardCostShare getAwardCostShare() {
		return awardCostShare;
	}

	public void setAwardCostShare(AwardCostShare awardCostShare) {
		this.awardCostShare = awardCostShare;
	}

	public String getUpdateType() {
		return updateType;
	}

	public void setUpdateType(String updateType) {
		this.updateType = updateType;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public Integer getAwardId() {
		return awardId;
	}

	public void setAwardId(Integer awardId) {
		this.awardId = awardId;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}
}
