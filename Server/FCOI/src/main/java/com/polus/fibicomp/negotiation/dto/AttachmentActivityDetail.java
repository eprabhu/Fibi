package com.polus.fibicomp.negotiation.dto;

import java.sql.Timestamp;

public class AttachmentActivityDetail {

	private String location;

	private String locationDescription;

	private String assignee;

	private Timestamp assignedOn;

	private String status;

	private String activityType;

	private String activityComment;

	private Timestamp startDate;

	private Timestamp endDate;

	private Timestamp followupDate;

	public String getLocation() {
		return location;
	}

	public void setLocation(String location) {
		this.location = location;
	}

	public String getLocationDescription() {
		return locationDescription;
	}

	public void setLocationDescription(String locationDescription) {
		this.locationDescription = locationDescription;
	}

	public String getAssignee() {
		return assignee;
	}

	public void setAssignee(String assignee) {
		this.assignee = assignee;
	}

	public Timestamp getAssignedOn() {
		return assignedOn;
	}

	public void setAssignedOn(Timestamp assignedOn) {
		this.assignedOn = assignedOn;
	}

	public String getStatus() {
		return status;
	}

	public void setStatus(String status) {
		this.status = status;
	}

	public String getActivityType() {
		return activityType;
	}

	public void setActivityType(String activityType) {
		this.activityType = activityType;
	}

	public String getActivityComment() {
		return activityComment;
	}

	public void setActivityComment(String activityComment) {
		this.activityComment = activityComment;
	}

	public Timestamp getStartDate() {
		return startDate;
	}

	public void setStartDate(Timestamp startDate) {
		this.startDate = startDate;
	}

	public Timestamp getEndDate() {
		return endDate;
	}

	public void setEndDate(Timestamp endDate) {
		this.endDate = endDate;
	}

	public Timestamp getFollowupDate() {
		return followupDate;
	}

	public void setFollowupDate(Timestamp followupDate) {
		this.followupDate = followupDate;
	}
}
