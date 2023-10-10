package com.polus.fibicomp.coi.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.List;

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
import javax.persistence.Transient;

import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import com.polus.fibicomp.util.JpaCharBooleanConversion;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Table(name = "DISCL_COMMENT")
@EntityListeners(AuditingEntityListener.class)
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
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

	@Column(name = "COMPONENT_SUB_REFERENCE_ID")
	private Integer componentSubReferenceId;

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

	@Transient
	private String updateUserFullName;

	@Transient
	private List<CoiReviewCommentTag> coiReviewCommentTag;

	@Transient
	private PersonEntity personEntity;

	@Transient
	private CoiDisclEntProjDetails disclEntProjDetails;

	@Transient
	private List<DisclAttachment> disclAttachments;

	@Transient
	private List<DisclComment> reply;

}
