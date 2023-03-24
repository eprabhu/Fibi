package com.polus.fibicomp.coi.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
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

@Entity
@Table(name = "COI_CONFLICT_HISTORY")
@EntityListeners(AuditingEntityListener.class)
public class CoiConflictHistory implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "COI_CONFLICT_HISTORY_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer coiConflictHistoryId;

	@Column(name = "DISC_DET_STATUS_CODE")
	private String coiDetStatusCode;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "COI_CONFLICT_HISTORY_FK1"), name = "DISC_DET_STATUS_CODE", referencedColumnName = "DISC_DET_STATUS_CODE", insertable = false, updatable = false)
	private CoiDisclosureDetailsStatus coiDisclosureDetailsStatus;

	@Column(name = "DISCLOSURE_DETAILS_ID")
	private Integer disclosureDetailsId;
	
	@Column(name = "COMMENT")
	private String comment;

	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;
	
	@Transient
	private String updateUserFullName;

	public Integer getCoiConflictHistoryId() {
		return coiConflictHistoryId;
	}

	public void setCoiConflictHistoryId(Integer coiConflictHistoryId) {
		this.coiConflictHistoryId = coiConflictHistoryId;
	}

	public String getCoiDetStatusCode() {
		return coiDetStatusCode;
	}

	public void setCoiDetStatusCode(String coiDetStatusCode) {
		this.coiDetStatusCode = coiDetStatusCode;
	}

	public CoiDisclosureDetailsStatus getCoiDisclosureDetailsStatus() {
		return coiDisclosureDetailsStatus;
	}

	public void setCoiDisclosureDetailsStatus(CoiDisclosureDetailsStatus coiDisclosureDetailsStatus) {
		this.coiDisclosureDetailsStatus = coiDisclosureDetailsStatus;
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

}
