package com.polus.fibicomp.award.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "AWARD_REVIEW_COMMENT")
public class AwardReviewComment implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "AWARD_REVIEW_COMMENT_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "AWARD_REVIEW_COMMENT_ID_GENERATOR")
	@SequenceGenerator(name="AWARD_REVIEW_COMMENT_ID_GENERATOR", sequenceName = "AWARD_REVIEW_COMMENT_ID_GENERATOR", allocationSize=1)
	private Integer awardReviewCommentId;

	@Column(name = "PARENT_REVIEW_COMMENT_ID")
	private Integer parentReviewCommentId;

	@Column(name = "AWARD_ID")
	private Integer awardId;

	@Column(name = "AWARD_NUMBER")
	private String awardNumber;

	@Column(name = "SEQUENCE_NUMBER")
	private Integer sequenceNumber;

	@Column(name = "REVIEW_COMMENT")
	private String reviewComment;

	@Column(name = "REVIEW_SECTION_CODE")
	private String reviewSectionCode;

	@Column(name = "REVIEW_COMMENT_TYPE_CODE")
	private String reviewCommentTypeCode;

	@Column(name = "IS_PRIVATE_COMMENT")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isPrivateComment = false;

	@Column(name = "REVIEWER_PERSON_ID")
	private String reviewerPersonId;

	@Column(name = "IS_RESOLVED")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isResolved = false;

	@Column(name = "RESOLVED_BY")
	private String resolvedBy;

	@Column(name = "RESOLVED_TIMESTAMP")
	private Timestamp resolvedTimeStamp;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Transient
	private String reviewerFullName;

	@Transient
	private String awardSequenceStatus;

	@Transient
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

	public Boolean getIsPrivateComment() {
		return isPrivateComment;
	}

	public void setIsPrivateComment(Boolean isPrivateComment) {
		this.isPrivateComment = isPrivateComment;
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

	public String getResolvedByFullName() {
		return resolvedByFullName;
	}

	public void setResolvedByFullName(String resolvedByFullName) {
		this.resolvedByFullName = resolvedByFullName;
	}

	public String getAwardSequenceStatus() {
		return awardSequenceStatus;
	}

	public void setAwardSequenceStatus(String awardSequenceStatus) {
		this.awardSequenceStatus = awardSequenceStatus;
	}

}
