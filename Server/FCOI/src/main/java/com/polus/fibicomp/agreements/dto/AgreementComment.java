package com.polus.fibicomp.agreements.dto;

import java.sql.Timestamp;
import java.util.List;
import java.util.Map;

public class AgreementComment {

	private Integer commentId;

	private String comment;

	private String commentType;

	private String locationName;

	private Integer negotiationLocationId;

	private String updateUser;

	private String updateUserFullName;

	private Timestamp updateTimestamp;

	private List<Map<Integer,String>> attachmentDetails;

	public Integer getCommentId() {
		return commentId;
	}

	public void setCommentId(Integer commentId) {
		this.commentId = commentId;
	}

	public String getComment() {
		return comment;
	}

	public void setComment(String comment) {
		this.comment = comment;
	}

	public String getLocationName() {
		return locationName;
	}

	public void setLocationName(String locationName) {
		this.locationName = locationName;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public String getUpdateUserFullName() {
		return updateUserFullName;
	}

	public void setUpdateUserFullName(String updateUserFullName) {
		this.updateUserFullName = updateUserFullName;
	}

	public String getCommentType() {
		return commentType;
	}

	public void setCommentType(String commentType) {
		this.commentType = commentType;
	}

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

	public List<Map<Integer, String>> getAttachmentDetails() {
		return attachmentDetails;
	}

	public void setAttachmentDetails(List<Map<Integer, String>> attachmentDetails) {
		this.attachmentDetails = attachmentDetails;
	}

	public Integer getNegotiationLocationId() {
		return negotiationLocationId;
	}

	public void setNegotiationLocationId(Integer negotiationLocationId) {
		this.negotiationLocationId = negotiationLocationId;
	}

}
