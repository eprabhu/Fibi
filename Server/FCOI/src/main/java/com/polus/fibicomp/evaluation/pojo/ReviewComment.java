package com.polus.fibicomp.evaluation.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
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
@Table(name = "PROPOSAL_REVIEW_COMMENT")
public class ReviewComment implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "REVIEW_COMMENT_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "PROP_EVAL_REVW_CMT_ID_GENERATOR")
	@SequenceGenerator(name="PROP_EVAL_REVW_CMT_ID_GENERATOR", sequenceName = "PROP_EVAL_REVW_CMT_ID_GENERATOR", allocationSize=1)
	private Integer commentId;

	@Column(name = "PROPOSAL_ID")
	private Integer proposalId;

	@JsonBackReference
	@ManyToOne(optional = false, cascade = CascadeType.REFRESH)
	@JoinColumn(foreignKey = @ForeignKey(name = "PROPOSAL_REVIEW_COMMENT_FK1"), name = "REVIEW_ID", referencedColumnName = "REVIEW_ID")
	private ProposalReview proposalReview;

	@Column(name = "REVIEW_COMMENT", length = 4000)
	private String reviewComment;

	@JsonManagedReference
	@OneToMany(mappedBy = "reviewComment", orphanRemoval = true, cascade = { CascadeType.REMOVE, CascadeType.ALL })
	private List<ReviewAttachment> reviewAttachments;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER", length = 60)
	private String updateUser;

	@Column(name = "IS_PRIVATE_COMMENT")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isPrivateComment = false;

	@Column(name = "PERSON_ID")
	private String personId;

	@Column(name = "FULL_NAME")
	private String fullName;

	public ReviewComment() {
		reviewAttachments = new ArrayList<>();
	}

	public Integer getCommentId() {
		return commentId;
	}

	public void setCommentId(Integer commentId) {
		this.commentId = commentId;
	}

	public Integer getProposalId() {
		return proposalId;
	}

	public void setProposalId(Integer proposalId) {
		this.proposalId = proposalId;
	}

	public ProposalReview getProposalReview() {
		return proposalReview;
	}

	public void setProposalReview(ProposalReview proposalReview) {
		this.proposalReview = proposalReview;
	}

	public String getReviewComment() {
		return reviewComment;
	}

	public void setReviewComment(String reviewComment) {
		this.reviewComment = reviewComment;
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

	public List<ReviewAttachment> getReviewAttachments() {
		return reviewAttachments;
	}

	public void setReviewAttachments(List<ReviewAttachment> reviewAttachments) {
		this.reviewAttachments = reviewAttachments;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public Boolean getIsPrivateComment() {
		return isPrivateComment;
	}

	public void setIsPrivateComment(Boolean isPrivateComment) {
		this.isPrivateComment = isPrivateComment;
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
