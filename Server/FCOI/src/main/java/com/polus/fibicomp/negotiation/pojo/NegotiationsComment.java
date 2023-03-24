package com.polus.fibicomp.negotiation.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
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
import javax.persistence.Transient;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.fasterxml.jackson.annotation.JsonManagedReference;

@Entity
@Table(name = "NEGOTIATION_COMMENT")
public class NegotiationsComment implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SEQ_NEGOTIATION_COMMENT")
	@SequenceGenerator(name = "SEQ_NEGOTIATION_COMMENT", sequenceName = "SEQ_NEGOTIATION_COMMENT", allocationSize = 1)
	@Column(name = "NEGOTIATION_COMMENT_ID")
	private Integer negotiationCommentId;

	@Column(name = "NEGOTIATION_ID")
	private Integer negotiationId;

	@JsonBackReference
	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "NEGOTIATION_COMMENT_FK2"), name = "NEGOTIATION_ID", referencedColumnName = "NEGOTIATION_ID", insertable = false, updatable = false)
	private Negotiations negotiations;

	@Column(name = "COMMENT_TYPE_CODE")
	private String commentTypeCode;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "NEGOTIATION_COMMENT_FK1"), name = "COMMENT_TYPE_CODE", referencedColumnName = "COMMENT_TYPE_CODE", insertable = false, updatable = false)
	private NegotiationsCommentType negotiationsCommentType;

	@Column(name = "COMMENTS")
	private String comments;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "NEGOTIATION_LOCATION_ID")
	private Integer negotiationLocationId;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "NEGOTIATION_COMMENT_FK4"), name = "NEGOTIATION_LOCATION_ID", referencedColumnName = "NEGOTIATION_LOCATION_ID", insertable = false, updatable = false)
	private NegotiationsLocation negotiationsLocation;

	@JsonManagedReference
	@OneToMany(mappedBy = "negotiationsComment", orphanRemoval = true, cascade = { CascadeType.REMOVE, CascadeType.ALL })
	private List<NegotiationCommentAttachment> negotiationCommentAttachment;

	@Transient
	private String acType;

	@Transient
	private String updateUserFullName;

	public NegotiationsComment() {
		negotiationCommentAttachment = new ArrayList<>();
	}

	public String getComments() {
		return comments;
	}

	public void setComments(String comments) {
		this.comments = comments;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

	public Negotiations getNegotiations() {
		return negotiations;
	}

	public void setNegotiations(Negotiations negotiations) {
		this.negotiations = negotiations;
	}

	public String getCommentTypeCode() {
		return commentTypeCode;
	}

	public void setCommentTypeCode(String commentTypeCode) {
		this.commentTypeCode = commentTypeCode;
	}

	public NegotiationsCommentType getNegotiationsCommentType() {
		return negotiationsCommentType;
	}

	public void setNegotiationsCommentType(NegotiationsCommentType negotiationsCommentType) {
		this.negotiationsCommentType = negotiationsCommentType;
	}

	public String getAcType() {
		if (acType == null) {
			return "U";
		}
		return acType;
	}

	public void setAcType(String acType) {
		this.acType = acType;
	}

	public Integer getNegotiationCommentId() {
		return negotiationCommentId;
	}

	public void setNegotiationCommentId(Integer negotiationCommentId) {
		this.negotiationCommentId = negotiationCommentId;
	}

	public Integer getNegotiationId() {
		return negotiationId;
	}

	public void setNegotiationId(Integer negotiationId) {
		this.negotiationId = negotiationId;
	}

	public Integer getNegotiationLocationId() {
		return negotiationLocationId;
	}

	public void setNegotiationLocationId(Integer negotiationLocationId) {
		this.negotiationLocationId = negotiationLocationId;
	}

	public String getUpdateUserFullName() {
		return updateUserFullName;
	}

	public void setUpdateUserFullName(String updateUserFullName) {
		this.updateUserFullName = updateUserFullName;
	}

	public NegotiationsLocation getNegotiationsLocation() {
		return negotiationsLocation;
	}

	public void setNegotiationsLocation(NegotiationsLocation negotiationsLocation) {
		this.negotiationsLocation = negotiationsLocation;
	}

	public List<NegotiationCommentAttachment> getNegotiationCommentAttachment() {
		return negotiationCommentAttachment;
	}

	public void setNegotiationCommentAttachment(List<NegotiationCommentAttachment> negotiationCommentAttachment) {
		this.negotiationCommentAttachment = negotiationCommentAttachment;
	}

}
