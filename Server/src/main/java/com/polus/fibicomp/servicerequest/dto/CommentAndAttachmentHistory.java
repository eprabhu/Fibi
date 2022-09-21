package com.polus.fibicomp.servicerequest.dto;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;

public class CommentAndAttachmentHistory {

	private Integer commentId;
	private String comments;
	private Timestamp updateTimeStamp;
	private String updateUser;
	private List<Attachment> attachments;
	private String actionTypeDescription;
	private String updateTimeStampInAMPM;
	private Boolean isPrivateComment = false;
	private String commentedUser;

	public CommentAndAttachmentHistory() {
		attachments = new ArrayList<>();
	}

	public Integer getCommentId() {
		return commentId;
	}

	public void setCommentId(Integer commentId) {
		this.commentId = commentId;
	}

	public String getComments() {
		return comments;
	}

	public void setComments(String comments) {
		this.comments = comments;
	}

	public Timestamp getUpdateTimeStamp() {
		return updateTimeStamp;
	}

	public void setUpdateTimeStamp(Timestamp updateTimeStamp) {
		this.updateTimeStamp = updateTimeStamp;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public List<Attachment> getAttachments() {
		return attachments;
	}

	public void setAttachments(List<Attachment> attachments) {
		this.attachments = attachments;
	}

	public String getActionTypeDescription() {
		return actionTypeDescription;
	}

	public void setActionTypeDescription(String actionTypeDescription) {
		this.actionTypeDescription = actionTypeDescription;
	}

	public String getUpdateTimeStampInAMPM() {
		return updateTimeStampInAMPM;
	}

	public void setUpdateTimeStampInAMPM(String updateTimeStampInAMPM) {
		this.updateTimeStampInAMPM = updateTimeStampInAMPM;
	}

	public Boolean getIsPrivateComment() {
		return isPrivateComment;
	}

	public void setIsPrivateComment(Boolean isPrivateComment) {
		this.isPrivateComment = isPrivateComment;
	}

	public String getCommentedUser() {
		return commentedUser;
	}

	public void setCommentedUser(String commentedUser) {
		this.commentedUser = commentedUser;
	}

}
