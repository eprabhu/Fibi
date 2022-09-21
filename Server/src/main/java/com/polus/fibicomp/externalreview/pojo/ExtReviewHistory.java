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
import javax.persistence.Transient;


@Entity
@Table(name = "EXT_REVIEW_HISTORY")
public class ExtReviewHistory implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
    @Column(name = "EXT_REVIEW_HISTORY_ID")
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Integer extReviewHistorytId;

	@Column(name = "EXT_REVIEW_ACTION_TYPE_CODE")
    private Integer extReviewActionTypeCode;

    @Column(name = "EXT_REVIEW_ID")
    private Integer extReviewId;

    @Column(name = "DESCRIPTION")
    private String description;

    @Column(name = "EXT_REVIEW_STATUS_CODE")
    private Integer extReviewStatusCode;

    @ManyToOne
    @JoinColumn(foreignKey = @ForeignKey(name = "EXT_REVIEW_HISTORY_FK_03"), name = "EXT_REVIEW_STATUS_CODE", referencedColumnName = "EXT_REVIEW_STATUS_CODE", insertable = false, updatable = false)
    private ExtReviewStatus extReviewStatus;

    @Column(name = "EXT_REVIEW_REVIEWERS_STATUS_CODE")
    private Integer extReviewReviewersStatusCode;

    @Column(name = "REVIEW_REVIEWER_ID")
    private Integer reviewReviewerId;

    @Column(name = "EXT_REV_REVIEWER_FLOW_STATUS_CODE")
    private String extRevReviewerFlowStatusCode;

    @Column(name = "UPDATE_TIMESTAMP")
    private Timestamp updateTimestamp;

    @Column(name = "UPDATE_USER")
    private String updateUser;

    @Transient
    private String updateUserFullName;

	public Integer getExtReviewHistorytId() {
		return extReviewHistorytId;
	}

	public void setExtReviewHistorytId(Integer extReviewHistorytId) {
		this.extReviewHistorytId = extReviewHistorytId;
	}

	public Integer getExtReviewActionTypeCode() {
		return extReviewActionTypeCode;
	}

	public void setExtReviewActionTypeCode(Integer extReviewActionTypeCode) {
		this.extReviewActionTypeCode = extReviewActionTypeCode;
	}

	public Integer getExtReviewId() {
		return extReviewId;
	}

	public void setExtReviewId(Integer extReviewId) {
		this.extReviewId = extReviewId;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public Integer getExtReviewStatusCode() {
		return extReviewStatusCode;
	}

	public void setExtReviewStatusCode(Integer extReviewStatusCode) {
		this.extReviewStatusCode = extReviewStatusCode;
	}

	public ExtReviewStatus getExtReviewStatus() {
		return extReviewStatus;
	}

	public void setExtReviewStatus(ExtReviewStatus extReviewStatus) {
		this.extReviewStatus = extReviewStatus;
	}

	public Integer getExtReviewReviewersStatusCode() {
		return extReviewReviewersStatusCode;
	}

	public void setExtReviewReviewersStatusCode(Integer extReviewReviewersStatusCode) {
		this.extReviewReviewersStatusCode = extReviewReviewersStatusCode;
	}

	public Integer getReviewReviewerId() {
		return reviewReviewerId;
	}

	public void setReviewReviewerId(Integer reviewReviewerId) {
		this.reviewReviewerId = reviewReviewerId;
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

	public String getUpdateUserFullName() {
		return updateUserFullName;
	}

	public void setUpdateUserFullName(String updateUserFullName) {
		this.updateUserFullName = updateUserFullName;
	}

}
