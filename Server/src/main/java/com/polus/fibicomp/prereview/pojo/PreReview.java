package com.polus.fibicomp.prereview.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.fasterxml.jackson.annotation.JsonManagedReference;

@Entity
@Table(name = "PRE_REVIEW")
public class PreReview implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "PRE_REVIEW_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "PRE_REVIEW_ID_GENERATOR")
	@SequenceGenerator(name="PRE_REVIEW_ID_GENERATOR", sequenceName = "PRE_REVIEW_ID_GENERATOR", allocationSize=1)
	private Integer preReviewId;

	@Column(name = "MODULE_ITEM_CODE")
	private Integer moduleItemCode;

	@Column(name = "MODULE_SUB_ITEM_CODE")
	private Integer moduleSubItemCode;

	@Column(name = "MODULE_ITEM_KEY")
	private String moduleItemKey;

	@Column(name = "MODULE_SUB_ITEM_KEY")
	private String moduleSubItemKey;

	@Column(name = "PRE_REVIEW_TYPE_CODE")
	private String reviewTypeCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "PRE_REVIEW_FK1"), name = "PRE_REVIEW_TYPE_CODE", referencedColumnName = "PRE_REVIEW_TYPE_CODE", insertable = false, updatable = false)
	private PreReviewType preReviewType;

	@Column(name = "PRE_REVIEW_SECTION_TYPE_CODE")
	private String reviewSectionTypeCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "PRE_REVIEW_FK3"), name = "PRE_REVIEW_SECTION_TYPE_CODE", referencedColumnName = "PRE_REVIEW_SECTION_TYPE_CODE", insertable = false, updatable = false)
	private PreReviewSectionType preReviewSectionType;

	@Column(name = "PRE_REVIEW_STATUS_CODE")
	private String reviewStatusCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "PRE_REVIEW_FK2"), name = "PRE_REVIEW_STATUS_CODE", referencedColumnName = "PRE_REVIEW_STATUS_CODE", insertable = false, updatable = false)
	private PreReviewStatus preReviewStatus;

	@Column(name = "REVIEWER_PERSON_ID")
	private String reviewerPersonId;

	@Column(name = "REVIEWER_FULLNAME")
	private String reviewerFullName;

	@Column(name = "REVIEWER_EMAIL")
	private String reviewerEmailAddress;

	@Column(name = "REQUESTOR_PERSON_ID")
	private String requestorPersonId;

	@Column(name = "REQUESTOR_FULLNAME")
	private String requestorFullName;

	@Column(name = "REQUESTOR_EMAIL")
	private String requestorEmailAddress;

	@Column(name = "REQUESTOR_COMMENT")
	private String requestorComment;

	@Column(name = "REQUEST_DATE")
	private Timestamp requestDate;

	@Column(name = "COMPLETION_DATE")
	private Timestamp completionDate;

	@JsonManagedReference
	@OneToMany(mappedBy = "preReview", orphanRemoval = true, cascade = { CascadeType.ALL }, fetch = FetchType.EAGER)
	private List<PreReviewComment> preReviewComments;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Transient
	private String moduleItemLabel;

	public PreReview() {
		preReviewComments = new ArrayList<PreReviewComment>();
	}

	public Integer getPreReviewId() {
		return preReviewId;
	}

	public void setPreReviewId(Integer preReviewId) {
		this.preReviewId = preReviewId;
	}

	public Integer getModuleItemCode() {
		return moduleItemCode;
	}

	public void setModuleItemCode(Integer moduleItemCode) {
		this.moduleItemCode = moduleItemCode;
	}

	public Integer getModuleSubItemCode() {
		return moduleSubItemCode;
	}

	public void setModuleSubItemCode(Integer moduleSubItemCode) {
		this.moduleSubItemCode = moduleSubItemCode;
	}

	public String getModuleItemKey() {
		return moduleItemKey;
	}

	public void setModuleItemKey(String moduleItemKey) {
		this.moduleItemKey = moduleItemKey;
	}

	public String getModuleSubItemKey() {
		return moduleSubItemKey;
	}

	public void setModuleSubItemKey(String moduleSubItemKey) {
		this.moduleSubItemKey = moduleSubItemKey;
	}

	public String getReviewTypeCode() {
		return reviewTypeCode;
	}

	public void setReviewTypeCode(String reviewTypeCode) {
		this.reviewTypeCode = reviewTypeCode;
	}

	public PreReviewType getPreReviewType() {
		return preReviewType;
	}

	public void setPreReviewType(PreReviewType preReviewType) {
		this.preReviewType = preReviewType;
	}

	public String getReviewSectionTypeCode() {
		return reviewSectionTypeCode;
	}

	public void setReviewSectionTypeCode(String reviewSectionTypeCode) {
		this.reviewSectionTypeCode = reviewSectionTypeCode;
	}

	public PreReviewSectionType getPreReviewSectionType() {
		return preReviewSectionType;
	}

	public void setPreReviewSectionType(PreReviewSectionType preReviewSectionType) {
		this.preReviewSectionType = preReviewSectionType;
	}

	public String getReviewStatusCode() {
		return reviewStatusCode;
	}

	public void setReviewStatusCode(String reviewStatusCode) {
		this.reviewStatusCode = reviewStatusCode;
	}

	public PreReviewStatus getPreReviewStatus() {
		return preReviewStatus;
	}

	public void setPreReviewStatus(PreReviewStatus preReviewStatus) {
		this.preReviewStatus = preReviewStatus;
	}

	public String getReviewerPersonId() {
		return reviewerPersonId;
	}

	public void setReviewerPersonId(String reviewerPersonId) {
		this.reviewerPersonId = reviewerPersonId;
	}

	public String getReviewerFullName() {
		return reviewerFullName;
	}

	public void setReviewerFullName(String reviewerFullName) {
		this.reviewerFullName = reviewerFullName;
	}

	public String getReviewerEmailAddress() {
		return reviewerEmailAddress;
	}

	public void setReviewerEmailAddress(String reviewerEmailAddress) {
		this.reviewerEmailAddress = reviewerEmailAddress;
	}

	public String getRequestorPersonId() {
		return requestorPersonId;
	}

	public void setRequestorPersonId(String requestorPersonId) {
		this.requestorPersonId = requestorPersonId;
	}

	public String getRequestorFullName() {
		return requestorFullName;
	}

	public void setRequestorFullName(String requestorFullName) {
		this.requestorFullName = requestorFullName;
	}

	public String getRequestorEmailAddress() {
		return requestorEmailAddress;
	}

	public void setRequestorEmailAddress(String requestorEmailAddress) {
		this.requestorEmailAddress = requestorEmailAddress;
	}

	public String getRequestorComment() {
		return requestorComment;
	}

	public void setRequestorComment(String requestorComment) {
		this.requestorComment = requestorComment;
	}

	public Timestamp getRequestDate() {
		return requestDate;
	}

	public void setRequestDate(Timestamp requestDate) {
		this.requestDate = requestDate;
	}

	public Timestamp getCompletionDate() {
		return completionDate;
	}

	public void setCompletionDate(Timestamp completionDate) {
		this.completionDate = completionDate;
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

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public List<PreReviewComment> getPreReviewComments() {
		return preReviewComments;
	}

	public void setPreReviewComments(List<PreReviewComment> preReviewComments) {
		this.preReviewComments = preReviewComments;
	}

	public String getModuleItemLabel() {
		return moduleItemLabel;
	}

	public void setModuleItemLabel(String moduleItemLabel) {
		this.moduleItemLabel = moduleItemLabel;
	}

}
