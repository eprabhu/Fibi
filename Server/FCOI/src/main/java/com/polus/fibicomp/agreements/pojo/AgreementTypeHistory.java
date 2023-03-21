package com.polus.fibicomp.agreements.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

@Entity
@Table(name = "AGREEMENT_TYPE_HISTORY")
public class AgreementTypeHistory implements Serializable{

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "AGREEMENT_TYPE_HISTORY_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SEQ_AGRMNT_TYPE_HISTORY")
	@SequenceGenerator(name="SEQ_AGRMNT_TYPE_HISTORY", sequenceName = "SEQ_AGRMNT_TYPE_HISTORY", allocationSize=1)
	private Integer agreementTypeHistoryId;

	@Column(name = "AGREEMENT_REQUEST_ID")
	private Integer agreementRequestId;

	@Column(name = "AGREEMENT_TYPE_CODE")
	private String agreementTypeCode;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "AGREEMENT_TYPE_HISTORY_FK2"), name = "AGREEMENT_TYPE_CODE", referencedColumnName = "AGREEMENT_TYPE_CODE", insertable = false, updatable = false)
	private AgreementType agreementType;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Integer getAgreementTypeHistoryId() {
		return agreementTypeHistoryId;
	}

	public void setAgreementTypeHistoryId(Integer agreementTypeHistoryId) {
		this.agreementTypeHistoryId = agreementTypeHistoryId;
	}

	public AgreementType getAgreementType() {
		return agreementType;
	}

	public void setAgreementType(AgreementType agreementType) {
		this.agreementType = agreementType;
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

	public Integer getAgreementRequestId() {
		return agreementRequestId;
	}

	public void setAgreementRequestId(Integer agreementRequestId) {
		this.agreementRequestId = agreementRequestId;
	}

	public String getAgreementTypeCode() {
		return agreementTypeCode;
	}

	public void setAgreementTypeCode(String agreementTypeCode) {
		this.agreementTypeCode = agreementTypeCode;
	}

}
