package com.polus.fibicomp.coi.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;
import javax.persistence.Table;

import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "COI_DISC_DETAILS_COMMENTS")
@EntityListeners(AuditingEntityListener.class)
public class CoiDisclosureDetailsComments implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "DISCLOSURE_DETAILS_COMMENT_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer disclosureDetailsCommentId;

	@JsonBackReference
	@OneToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "COI_DISC_DETAILS_COMMENTS_FK1"), name = "DISCLOSURE_DETAILS_ID", referencedColumnName = "DISCLOSURE_DETAILS_ID")
	private CoiDisclosureDetails coiDisclosureDetails;

	@Column(name = "DISCLOSURE_NUMBER")
	private String disclosureNumber;
	
	@Column(name = "COMMENT_TYPE_CODE")
	private String commentTypeCode;

	@Column(name = "COMMENT")
	private String comments;

	@Convert(converter = JpaCharBooleanConversion.class)
	@Column(name = "IS_PRIVATE")
	private Boolean isPrivate = false;
	
	@Column(name = "PARENT_DISC_DET_COMMENT_ID")
	private Integer parentDiscDetCommentId;

	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Integer getDisclosureDetailsCommentId() {
		return disclosureDetailsCommentId;
	}

	public void setDisclosureDetailsCommentId(Integer disclosureDetailsCommentId) {
		this.disclosureDetailsCommentId = disclosureDetailsCommentId;
	}

	public CoiDisclosureDetails getCoiDisclosureDetails() {
		return coiDisclosureDetails;
	}

	public void setCoiDisclosureDetails(CoiDisclosureDetails coiDisclosureDetails) {
		this.coiDisclosureDetails = coiDisclosureDetails;
	}

	public String getDisclosureNumber() {
		return disclosureNumber;
	}

	public void setDisclosureNumber(String disclosureNumber) {
		this.disclosureNumber = disclosureNumber;
	}

	public String getCommentTypeCode() {
		return commentTypeCode;
	}

	public void setCommentTypeCode(String commentTypeCode) {
		this.commentTypeCode = commentTypeCode;
	}

	public String getComments() {
		return comments;
	}

	public void setComments(String comments) {
		this.comments = comments;
	}

	public Boolean getIsPrivate() {
		return isPrivate;
	}

	public void setIsPrivate(Boolean isPrivate) {
		this.isPrivate = isPrivate;
	}

	public Integer getParentDiscDetCommentId() {
		return parentDiscDetCommentId;
	}

	public void setParentDiscDetCommentId(Integer parentDiscDetCommentId) {
		this.parentDiscDetCommentId = parentDiscDetCommentId;
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

}
