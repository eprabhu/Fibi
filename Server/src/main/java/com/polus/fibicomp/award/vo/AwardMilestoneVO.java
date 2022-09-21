package com.polus.fibicomp.award.vo;

import java.util.ArrayList;
import java.util.List;

import com.polus.fibicomp.award.pojo.AwardMileStone;

public class AwardMilestoneVO {

	private Integer awardId;

	private List<AwardMileStone> awardMileStones;

	private AwardMileStone awardMilestone;

	private String updateUser;

	private Integer awardMilestoneId;

	private String message;
	
	public AwardMilestoneVO() {
		awardMileStones = new ArrayList<AwardMileStone>();
	}

	public Integer getAwardId() {
		return awardId;
	}

	public void setAwardId(Integer awardId) {
		this.awardId = awardId;
	}

	public List<AwardMileStone> getAwardMileStones() {
		return awardMileStones;
	}

	public void setAwardMileStones(List<AwardMileStone> awardMileStones) {
		this.awardMileStones = awardMileStones;
	}

	public AwardMileStone getAwardMilestone() {
		return awardMilestone;
	}

	public void setAwardMilestone(AwardMileStone awardMilestone) {
		this.awardMilestone = awardMilestone;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public Integer getAwardMilestoneId() {
		return awardMilestoneId;
	}

	public void setAwardMilestoneId(Integer awardMilestoneId) {
		this.awardMilestoneId = awardMilestoneId;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}
	
}
