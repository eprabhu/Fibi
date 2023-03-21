package com.polus.fibicomp.externalreview.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import com.polus.fibicomp.externalreviewer.pojo.ExternalReviewer;

@Entity
@Table(name = "EXT_REVIEW_REVIEWERS")
public class ExtReviewReviewer implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
    @Column(name = "REVIEW_REVIEWER_ID")
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Integer reviewReviewerId;

	@Column(name = "EXTERNAL_REVIEWER_ID")
	private Integer externalReviewerId;

	@ManyToOne
    @JoinColumn(foreignKey = @ForeignKey(name = "EXT_REVIEW_REVIEWERS_FK_01"), name = "EXTERNAL_REVIEWER_ID", referencedColumnName = "EXTERNAL_REVIEWER_ID", insertable = false, updatable = false)
    private ExternalReviewer externalReviewer;

	@Column(name = "EXT_REVIEW_ID")
    private Integer extReviewId;

    @Column(name = "EXT_REVIEW_REVIEWERS_STATUS_CODE")
    private Integer extReviewReviewersStatusCode;

    @Column(name = "REVIEW_START_DATE")
    private Timestamp reviewStartDate;

    @Column(name = "COMMENT")
	private String comment;

    @Column(name = "REVIEW_REQUESTOR_ID")
    private String reviewRequestorId;

    @Column(name = "REVIEW_COMPLETED_DATE")
    private Timestamp reviewCompletedDate;

    @Column(name = "EXT_REV_REVIEWER_FLOW_STATUS_CODE")
    private String extRevReviewerFlowStatusCode;

    @Column(name = "UPDATE_TIMESTAMP")
    private Timestamp updateTimestamp;

    @Column(name = "UPDATE_USER")
    private String updateUser;

    @Column(name = "EXT_REVIEWERS_STATUS_CODE")
    private String extReviewersStatusCode;

	public Integer getReviewReviewerId() {
		return reviewReviewerId;
	}

	public void setReviewReviewerId(Integer reviewReviewerId) {
		this.reviewReviewerId = reviewReviewerId;
	}

	public Integer getExternalReviewerId() {
		return externalReviewerId;
	}

	public void setExternalReviewerId(Integer externalReviewerId) {
		this.externalReviewerId = externalReviewerId;
	}

	public ExternalReviewer getExternalReviewer() {
		return externalReviewer;
	}

	public void setExternalReviewer(ExternalReviewer externalReviewer) {
		this.externalReviewer = externalReviewer;
	}

	public Integer getExtReviewId() {
		return extReviewId;
	}

	public void setExtReviewId(Integer extReviewId) {
		this.extReviewId = extReviewId;
	}

	public Integer getExtReviewReviewersStatusCode() {
		return extReviewReviewersStatusCode;
	}

	public void setExtReviewReviewersStatusCode(Integer extReviewReviewersStatusCode) {
		this.extReviewReviewersStatusCode = extReviewReviewersStatusCode;
	}

	public Timestamp getReviewStartDate() {
		return reviewStartDate;
	}

	public void setReviewStartDate(Timestamp reviewStartDate) {
		this.reviewStartDate = reviewStartDate;
	}

	public String getComment() {
		return comment;
	}

	public void setComment(String comment) {
		this.comment = comment;
	}

	public String getReviewRequestorId() {
		return reviewRequestorId;
	}

	public void setReviewRequestorId(String reviewRequestorId) {
		this.reviewRequestorId = reviewRequestorId;
	}

	public Timestamp getReviewCompletedDate() {
		return reviewCompletedDate;
	}

	public void setReviewCompletedDate(Timestamp reviewCompletedDate) {
		this.reviewCompletedDate = reviewCompletedDate;
	}

	public String getExtRevReviewerFlowStatusCode() {
		return extRevReviewerFlowStatusCode;
	}

	public void setExtRevReviewerFlowStatusCode(String extRevReviewerFlowStatusCode) {
		this.extRevReviewerFlowStatusCode = extRevReviewerFlowStatusCode;
	}

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public String getExtReviewersStatusCode() {
		return extReviewersStatusCode;
	}

	public void setExtReviewersStatusCode(String extReviewersStatusCode) {
		this.extReviewersStatusCode = extReviewersStatusCode;
	}

}
