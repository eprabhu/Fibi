package com.polus.fibicomp.manpowerintegration.dto;

public class AwardClosePositionResorceDto {

	private Integer awardId;

	private String awardNumber;

	private String positionId;

	private String positionOwned;

	private String school;

	public AwardClosePositionResorceDto(Integer awardId, String awardNumber, String positionId, String positionOwned, String school) {
		super();
		this.awardId = awardId;
		this.awardNumber = awardNumber;
		this.positionId = positionId;
		this.positionOwned = positionOwned;
		this.school = school;
	}

	public Integer getAwardId() {
		return awardId;
	}

	public void setAwardId(Integer awardId) {
		this.awardId = awardId;
	}

	public String getAwardNumber() {
		return awardNumber;
	}

	public void setAwardNumber(String awardNumber) {
		this.awardNumber = awardNumber;
	}

	public String getPositionId() {
		return positionId;
	}

	public void setPositionId(String positionId) {
		this.positionId = positionId;
	}

	public String getPositionOwned() {
		return positionOwned;
	}

	public void setPositionOwned(String positionOwned) {
		this.positionOwned = positionOwned;
	}

	public String getSchool() {
		return school;
	}

	public void setSchool(String school) {
		this.school = school;
	}

}
