package com.polus.fibicomp.coi.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "DISCL_COMMENT")
@EntityListeners(AuditingEntityListener.class)
public class DisclComment implements Serializable {
	
	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "COMMENT_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer commentId;
	
	@Column(name = "COMPONENT_TYPE_CODE")
	private String componentTypeCode;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "DISCL_COMMENT_FK1"), name = "COMPONENT_TYPE_CODE", referencedColumnName = "COMPONENT_TYPE_CODE", insertable = false, updatable = false)
	private DisclComponentType disclComponentType;
	
	@Column(name = "COMPONENT_REFERENCE_ID")
	private Integer componentReferenceId;
	
	@Column(name = "COMPONENT_REFERENCE_NUMBER")
	private String componentReferenceNumber;
	
	@Column(name = "COMMENT_TYPE")
	private String commentType;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "DISCL_COMMENT_FK2"), name = "COMMENT_TYPE", referencedColumnName = "COMMENT_TYPE", insertable = false, updatable = false)
	private DisclCommentType disclCommentType;
	
	@Column(name = "COMMENT_BY_PERSON_ID")
	private String commentPersonId;
	
	@Column(name = "DOCUMENT_OWNER_PERSON_ID")
	private String documentOwnerPersonId;
	
	@Column(name = "IS_Private")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isPrivate;
	
	@Column(name = "COMMENT")
	private String comment;
	
	@Column(name = "PARENT_COMMENT_ID")
	private Integer parentCommentId;
	
	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;
	
	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	public DisclComponentType getDisclComponentType() {
		return disclComponentType;
	}

	public void setDisclComponentType(DisclComponentType disclComponentType) {
		this.disclComponentType = disclComponentType;
	}

	public DisclCommentType getDisclCommentType() {
		return disclCommentType;
	}

	public void setDisclCommentType(DisclCommentType disclCommentType) {
		this.disclCommentType = disclCommentType;
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

	public Integer getCommentId() {
		return commentId;
	}

	public void setCommentId(Integer commentId) {
		this.commentId = commentId;
	}

	public String getComponentTypeCode() {
		return componentTypeCode;
	}

	public void setComponentTypeCode(String componentTypeCode) {
		this.componentTypeCode = componentTypeCode;
	}

	public Integer getComponentReferenceId() {
		return componentReferenceId;
	}

	public void setComponentReferenceId(Integer componentReferenceId) {
		this.componentReferenceId = componentReferenceId;
	}

	public String getComponentReferenceNumber() {
		return componentReferenceNumber;
	}

	public void setComponentReferenceNumber(String componentReferenceNumber) {
		this.componentReferenceNumber = componentReferenceNumber;
	}

	public String getCommentType() {
		return commentType;
	}

	public void setCommentType(String commentType) {
		this.commentType = commentType;
	}

	public String getCommentPersonId() {
		return commentPersonId;
	}

	public void setCommentPersonId(String commentPersonId) {
		this.commentPersonId = commentPersonId;
	}

	public String getDocumentOwnerPersonId() {
		return documentOwnerPersonId;
	}

	public void setDocumentOwnerPersonId(String documentOwnerPersonId) {
		this.documentOwnerPersonId = documentOwnerPersonId;
	}

	public Boolean getIsPrivate() {
		return isPrivate;
	}

	public void setIsPrivate(Boolean isPrivate) {
		this.isPrivate = isPrivate;
	}

	public String getComment() {
		return comment;
	}

	public void setComment(String comment) {
		this.comment = comment;
	}

	public Integer getParentCommentId() {
		return parentCommentId;
	}

	public void setParentCommentId(Integer parentCommentId) {
		this.parentCommentId = parentCommentId;
	}
	
}
