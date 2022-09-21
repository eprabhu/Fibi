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
@Table(name = "AGREEMENT_CLAUSES")
public class AgreementClauses implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SEQ_AGREEMENT_CLAUSES")
	@SequenceGenerator(name="SEQ_AGREEMENT_CLAUSES", sequenceName = "SEQ_AGREEMENT_CLAUSES", allocationSize=1)
	@Column(name = "AGREEMENT_CLAUSES_ID")
	private Integer agreementClauseId;

	@Column(name = "AGREEMENT_REQUEST_ID")
	private Integer agreementRequestId;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "AGREEMENT_TYPE_CLAUSES_FK1"), name = "AGREEMENT_REQUEST_ID", referencedColumnName = "AGREEMENT_REQUEST_ID", insertable = false, updatable = false)
	private AgreementHeader agreementHeader;

	@Column(name = "CLAUSES")
	private String clauses;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "CLAUSES_GROUP_CODE")
	private Integer clausesGroupCode;

	@ManyToOne
	@JoinColumn(foreignKey = @ForeignKey(name = "AGREEMENT_CLAUSES_FK1"), name = "CLAUSES_GROUP_CODE", referencedColumnName = "CLAUSES_GROUP_CODE", insertable = false, updatable = false)
	private ClausesGroup clausesGroup;

	public Integer getAgreementClauseId() {
		return agreementClauseId;
	}

	public void setAgreementClauseId(Integer agreementClauseId) {
		this.agreementClauseId = agreementClauseId;
	}

	public Integer getAgreementRequestId() {
		return agreementRequestId;
	}

	public void setAgreementRequestId(Integer agreementRequestId) {
		this.agreementRequestId = agreementRequestId;
	}

	public AgreementHeader getAgreementHeader() {
		return agreementHeader;
	}

	public void setAgreementHeader(AgreementHeader agreementHeader) {
		this.agreementHeader = agreementHeader;
	}

	public String getClauses() {
		return clauses;
	}

	public void setClauses(String clauses) {
		this.clauses = clauses;
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

	public Integer getClausesGroupCode() {
		return clausesGroupCode;
	}

	public void setClausesGroupCode(Integer clausesGroupCode) {
		this.clausesGroupCode = clausesGroupCode;
	}

	public ClausesGroup getClausesGroup() {
		return clausesGroup;
	}

	public void setClausesGroup(ClausesGroup clausesGroup) {
		this.clausesGroup = clausesGroup;
	}

}
