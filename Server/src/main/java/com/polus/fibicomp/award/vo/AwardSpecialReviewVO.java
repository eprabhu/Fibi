package com.polus.fibicomp.award.vo;

import com.polus.fibicomp.award.pojo.AwardSpecialReview;

public class AwardSpecialReviewVO {

	private Integer awardSpecailReviewId;

	private AwardSpecialReview specialReview;

	private String updateType;

	private String message;

	private String personId;

	private Integer awardId;

	private String updateUser;

	public AwardSpecialReviewVO() {
		this.specialReview = new AwardSpecialReview();
		this.updateType = "SAVE";
		this.message = "";
		this.personId = "10000000001";
	}

	public AwardSpecialReview getSpecialReview() {
		return specialReview;
	}

	public void setSpecialReview(AwardSpecialReview specialReview) {
		this.specialReview = specialReview;
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

	public Integer getAwardSpecailReviewId() {
		return awardSpecailReviewId;
	}

	public void setAwardSpecailReviewId(Integer awardSpecailReviewId) {
		this.awardSpecailReviewId = awardSpecailReviewId;
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
