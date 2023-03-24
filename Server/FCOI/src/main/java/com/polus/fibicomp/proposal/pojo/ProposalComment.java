package com.polus.fibicomp.proposal.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
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

import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import com.fasterxml.jackson.annotation.JsonManagedReference;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "EPS_PROPOSAL_COMMENTS")
@EntityListeners(AuditingEntityListener.class)
public class ProposalComment implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "EPS_PROPOSAL_COMMENT_ID", length = 10)
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "EPS_PROPOSAL_COMMENTS_ID_GENERATOR")
	@SequenceGenerator(name = "EPS_PROPOSAL_COMMENTS_ID_GENERATOR", sequenceName = "EPS_PROPOSAL_COMMENTS_ID_GENERATOR", allocationSize = 1)
	private Integer proposalCommentId;

	@Column(name = "PROPOSAL_ID")
	private Integer proposalId;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "EPS_PROPOSAL_COMMENTS_FK1"), name = "PROPOSAL_ID", referencedColumnName = "PROPOSAL_ID", insertable = false, updatable = false)
	private Proposal proposal;

	@Column(name = "COMMENT_TYPE_CODE")
	private String commentTypeCode;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "EPS_PROPOSAL_COMMENTS_FK2"), name = "COMMENT_TYPE_CODE", referencedColumnName = "COMMENT_TYPE_CODE", insertable = false, updatable = false)
	private CommentType commentType;

	@Column(name = "COMMENTS")
	private String comment;

	@Convert(converter = JpaCharBooleanConversion.class)
	@Column(name = "IS_PRIVATE")
	private Boolean isPrivate = false;

	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;

	@JsonManagedReference
	@OneToMany(mappedBy = "proposalComment", orphanRemoval = true, cascade = {CascadeType.ALL }, fetch = FetchType.LAZY)
	private List<ProposalCommentAttachment> proposalCommentAttachments;

	@Transient
	private String fullName;

	public ProposalComment() {
		proposalCommentAttachments = new ArrayList<>();
	}
	
	public Integer getProposalCommentId() {
		return proposalCommentId;
	}

	public void setProposalCommentId(Integer proposalCommentId) {
		this.proposalCommentId = proposalCommentId;
	}

	public Integer getProposalId() {
		return proposalId;
	}

	public void setProposalId(Integer proposalId) {
		this.proposalId = proposalId;
	}

	public Proposal getProposal() {
		return proposal;
	}

	public void setProposal(Proposal proposal) {
		this.proposal = proposal;
	}

	public CommentType getCommentType() {
		return commentType;
	}

	public void setCommentType(CommentType commentType) {
		this.commentType = commentType;
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

	public List<ProposalCommentAttachment> getProposalCommentAttachments() {
		return proposalCommentAttachments;
	}

	public void setProposalCommentAttachments(List<ProposalCommentAttachment> proposalCommentAttachments) {
		this.proposalCommentAttachments = proposalCommentAttachments;
	}

	public String getCommentTypeCode() {
		return commentTypeCode;
	}

	public void setCommentTypeCode(String commentTypeCode) {
		this.commentTypeCode = commentTypeCode;
	}

	public String getComment() {
		return comment;
	}

	public void setComments(String comment) {
		this.comment = comment;
	}

	public Boolean getIsPrivate() {
		return isPrivate;
	}

	public void setIsPrivate(Boolean isPrivate) {
		this.isPrivate = isPrivate;
	}

	public String getFullName() {
		return fullName;
	}

	public void setFullName(String fullName) {
		this.fullName = fullName;
	}

}
