package com.polus.fibicomp.award.awardreviewcomment.dto;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;

public class AwardReviewCommentDto {

	private Integer awardReviewCommentId;

	private Integer parentReviewCommentId;

	private Integer awardId;

	private String awardNumber;

	private Integer sequenceNumber;

	private String reviewComment;

	private String reviewSectionCode;

	private String reviewCommentTypeCode;

	private Boolean isPrivateComment = false;

	private String reviewerPersonId;

	private Boolean isResolved = false;

	private String resolvedBy;

	private Timestamp resolvedTimeStamp;

	private Timestamp updateTimeStamp;

	private String updateUser;

	private String reviewerFullName;
	
	private List<AwardReviewCommentDto> replies = new ArrayList<AwardReviewCommentDto>();

	private String resolvedByFullName;

	public Integer getAwardReviewCommentId() {
		return awardReviewCommentId;
	}

	public void setAwardReviewCommentId(Integer awardReviewCommentId) {
		this.awardReviewCommentId = awardReviewCommentId;
	}

	public Integer getParentReviewCommentId() {
		return parentReviewCommentId;
	}

	public void setParentReviewCommentId(Integer parentReviewCommentId) {
		this.parentReviewCommentId = parentReviewCommentId;
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

	public Integer getSequenceNumber() {
		return sequenceNumber;
	}

	public void setSequenceNumber(Integer sequenceNumber) {
		this.sequenceNumber = sequenceNumber;
	}

	public String getReviewComment() {
		return reviewComment;
	}

	public void setReviewComment(String reviewComment) {
		this.reviewComment = reviewComment;
	}

	public String getReviewSectionCode() {
		return reviewSectionCode;
	}

	public void setReviewSectionCode(String reviewSectionCode) {
		this.reviewSectionCode = reviewSectionCode;
	}

	public String getReviewCommentTypeCode() {
		return reviewCommentTypeCode;
	}

	public void setReviewCommentTypeCode(String reviewCommentTypeCode) {
		this.reviewCommentTypeCode = reviewCommentTypeCode;
	}

	public String getReviewerPersonId() {
		return reviewerPersonId;
	}

	public void setReviewerPersonId(String reviewerPersonId) {
		this.reviewerPersonId = reviewerPersonId;
	}

	public Boolean getIsResolved() {
		return isResolved;
	}

	public void setIsResolved(Boolean isResolved) {
		this.isResolved = isResolved;
	}

	public String getResolvedBy() {
		return resolvedBy;
	}

	public void setResolvedBy(String resolvedBy) {
		this.resolvedBy = resolvedBy;
	}

	public Timestamp getResolvedTimeStamp() {
		return resolvedTimeStamp;
	}

	public void setResolvedTimeStamp(Timestamp resolvedTimeStamp) {
		this.resolvedTimeStamp = resolvedTimeStamp;
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

	public String getReviewerFullName() {
		return reviewerFullName;
	}

	public void setReviewerFullName(String reviewerFullName) {
		this.reviewerFullName = reviewerFullName;
	}

	public List<AwardReviewCommentDto> getReplies() {
		return replies;
	}

	public void setReplies(List<AwardReviewCommentDto> replies) {
		this.replies = replies;
	}

	public String getResolvedByFullName() {
		return resolvedByFullName;
	}

	public void setResolvedByFullName(String resolvedByFullName) {
		this.resolvedByFullName = resolvedByFullName;
	}

	public Boolean getIsPrivateComment() {
		return isPrivateComment;
	}

	public void setIsPrivateComment(Boolean isPrivateComment) {
		this.isPrivateComment = isPrivateComment;
	}

}
