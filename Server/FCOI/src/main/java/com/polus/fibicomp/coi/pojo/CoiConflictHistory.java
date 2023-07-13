package com.polus.fibicomp.coi.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

@Entity
@Table(name = "COI_CONFLICT_HISTORY")
@EntityListeners(AuditingEntityListener.class)
public class CoiConflictHistory implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "COI_CONFLICT_HISTORY_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer coiConflictHistoryId;

	@Column(name = "DISCLOSURE_DETAILS_ID")
	private Integer disclosureDetailsId;
	
	@Column(name = "DISCLOSURE_ID")
	private Integer disclosureId;
	
	@Column(name = "COMMENT")
	private String comment;
	
	@Column(name = "CONFLICT_STATUS_CODE")
	private String conflictStatusCode;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;
	
	@Transient
	private String updateUserFullName;
	
	@Transient
	private String conflictStatusDescription;

	public Integer getCoiConflictHistoryId() {
		return coiConflictHistoryId;
	}

	public void setCoiConflictHistoryId(Integer coiConflictHistoryId) {
		this.coiConflictHistoryId = coiConflictHistoryId;
	}

	public Integer getDisclosureDetailsId() {
		return disclosureDetailsId;
	}

	public void setDisclosureDetailsId(Integer disclosureDetailsId) {
		this.disclosureDetailsId = disclosureDetailsId;
	}

	public String getComment() {
		return comment;
	}

	public void setComment(String comment) {
		this.comment = comment;
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

	public Integer getDisclosureId() {
		return disclosureId;
	}

	public void setDisclosureId(Integer disclosureId) {
		this.disclosureId = disclosureId;
	}

	public String getConflictStatusCode() {
		return conflictStatusCode;
	}

	public void setConflictStatusCode(String conflictStatusCode) {
		this.conflictStatusCode = conflictStatusCode;
	}

	public String getConflictStatusDescription() {
		return conflictStatusDescription;
	}

	public void setConflictStatusDescription(String conflictStatusDescription) {
		this.conflictStatusDescription = conflictStatusDescription;
	}

}
