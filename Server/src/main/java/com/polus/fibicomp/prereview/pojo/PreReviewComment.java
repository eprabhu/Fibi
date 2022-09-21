package com.polus.fibicomp.prereview.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Convert;
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

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.fasterxml.jackson.annotation.JsonManagedReference;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "PRE_REVIEW_COMMENT")
public class PreReviewComment implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "PRE_REVIEW_COMMENT_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "PRE_REVIEW_COMMENT_ID_GENERATOR")
	@SequenceGenerator(name="PRE_REVIEW_COMMENT_ID_GENERATOR", sequenceName = "PRE_REVIEW_COMMENT_ID_GENERATOR", allocationSize=1)
	private Integer preReviewCommentId;

	@JsonBackReference
	@ManyToOne(optional = false, fetch = FetchType.EAGER)
	@JoinColumn(foreignKey = @ForeignKey(name = "PRE_REVIEW_COMMENT_FK1"), name = "PRE_REVIEW_ID", referencedColumnName = "PRE_REVIEW_ID")
	private PreReview preReview;

	@Column(name = "REVIEW_COMMENT")
	private String reviewComment;

	@JsonManagedReference
	@OneToMany(mappedBy = "preReviewComment", orphanRemoval = true, cascade = { CascadeType.ALL }, fetch = FetchType.EAGER)
	private List<PreReviewAttachment> preReviewAttachments;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "IS_PRIVATE_COMMENT")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isPrivateComment = false;

	@Column(name = "PERSON_ID")
	private String personId;

	@Column(name = "FULL_NAME")
	private String fullName;

	public PreReviewComment() {
		super();
		preReviewAttachments = new ArrayList<>();
	}

	public Integer getPreReviewCommentId() {
		return preReviewCommentId;
	}

	public void setPreReviewCommentId(Integer preReviewCommentId) {
		this.preReviewCommentId = preReviewCommentId;
	}

	public String getReviewComment() {
		return reviewComment;
	}

	public void setReviewComment(String reviewComment) {
		this.reviewComment = reviewComment;
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

	public Boolean getIsPrivateComment() {
		return isPrivateComment;
	}

	public void setIsPrivateComment(Boolean isPrivateComment) {
		this.isPrivateComment = isPrivateComment;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public PreReview getPreReview() {
		return preReview;
	}

	public void setPreReview(PreReview preReview) {
		this.preReview = preReview;
	}

	public List<PreReviewAttachment> getPreReviewAttachments() {
		return preReviewAttachments;
	}

	public void setPreReviewAttachments(List<PreReviewAttachment> preReviewAttachments) {
		this.preReviewAttachments = preReviewAttachments;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public String getFullName() {
		return fullName;
	}

	public void setFullName(String fullName) {
		this.fullName = fullName;
	}

}
