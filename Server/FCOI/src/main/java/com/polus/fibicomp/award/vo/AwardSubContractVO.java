package com.polus.fibicomp.award.vo;

import com.polus.fibicomp.award.pojo.AwardSubContract;

public class AwardSubContractVO {

	private Integer awardSubawardId;

	private AwardSubContract subContract;

	private String updateType;

	private String message;

	private String personId;

	private Integer awardId;

	private String updateUser;

	public AwardSubContractVO() {
		this.subContract = new AwardSubContract();
		this.updateType = "SAVE";
		this.personId = "10000000001";
	}

	public Integer getAwardSubawardId() {
		return awardSubawardId;
	}

	public void setAwardSubawardId(Integer awardSubawardId) {
		this.awardSubawardId = awardSubawardId;
	}

	public AwardSubContract getSubContract() {
		return subContract;
	}

	public void setSubContract(AwardSubContract subContract) {
		this.subContract = subContract;
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
