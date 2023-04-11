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

import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

@Entity
@Table(name = "COI_DISCLOSURE_AUDIT_TRAIL")
@EntityListeners(AuditingEntityListener.class)
public class CoiDisclosureOldAuditTrail implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "COI_DISCLOSURE_AUDIT_TRAIL_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer CoiDisclosureOldAuditTrailId;
	
	@Column(name = "DISCLOSURE_ID")
	private Integer disclosureId;

	@Column(name = "COI_AUDIT_ACTION_TYPE_CODE")
	private String auditActionTypeCode;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "COI_DISCLOSURE_AUDIT_TRAIL_FK1"), name = "COI_AUDIT_ACTION_TYPE_CODE", referencedColumnName = "COI_AUDIT_ACTION_TYPE_CODE", insertable = false, updatable = false)
	private CoiAuditActionType coiAuditActionType;

	@Column(name = "SYSTEM_REMARK")
	private String systemRemark;

	@Column(name = "ADDITIONAL_REMARK")
	private String additionalRemark;

	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Integer getCoiDisclosureOldAuditTrailId() {
		return CoiDisclosureOldAuditTrailId;
	}

	public void setCoiDisclosureOldAuditTrailId(Integer CoiDisclosureOldAuditTrailId) {
		this.CoiDisclosureOldAuditTrailId = CoiDisclosureOldAuditTrailId;
	}

	public Integer getDisclosureId() {
		return disclosureId;
	}

	public void setDisclosureId(Integer disclosureId) {
		this.disclosureId = disclosureId;
	}

	public String getAuditActionTypeCode() {
		return auditActionTypeCode;
	}

	public void setAuditActionTypeCode(String auditActionTypeCode) {
		this.auditActionTypeCode = auditActionTypeCode;
	}

	public CoiAuditActionType getCoiAuditActionType() {
		return coiAuditActionType;
	}

	public void setCoiAuditActionType(CoiAuditActionType coiAuditActionType) {
		this.coiAuditActionType = coiAuditActionType;
	}

	public String getSystemRemark() {
		return systemRemark;
	}

	public void setSystemRemark(String systemRemark) {
		this.systemRemark = systemRemark;
	}

	public String getAdditionalRemark() {
		return additionalRemark;
	}

	public void setAdditionalRemark(String additionalRemark) {
		this.additionalRemark = additionalRemark;
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
