package com.polus.fibicomp.agreements.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.OneToMany;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.fasterxml.jackson.annotation.JsonManagedReference;

@Entity
@Table(name = "CLAUSES_GROUP")
public class ClausesGroup implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SEQ_CLAUSES_GROUP")
	@SequenceGenerator(name="SEQ_CLAUSES_GROUP", sequenceName = "SEQ_CLAUSES_GROUP", allocationSize=1)
	@Column(name = "CLAUSES_GROUP_CODE")
	private Integer clauseGroupCode;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@JsonManagedReference
	@OneToMany(mappedBy = "clausesGroup", orphanRemoval = true, cascade = { CascadeType.ALL }, fetch = FetchType.LAZY)
	private List<Clauses> clauses;

	@Transient
	private List<AgreementType> agreementTypes;

	public ClausesGroup() {
		agreementTypes = new ArrayList<>();
		clauses = new ArrayList<>();
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
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

	public List<Clauses> getClauses() {
		return clauses;
	}

	public void setClauses(List<Clauses> clauses) {
		this.clauses = clauses;
	}

	public List<AgreementType> getAgreementTypes() {
		return agreementTypes;
	}

	public void setAgreementTypes(List<AgreementType> agreementTypes) {
		this.agreementTypes = agreementTypes;
	}

	public Integer getClauseGroupCode() {
		return clauseGroupCode;
	}

	public void setClauseGroupCode(Integer clauseGroupCode) {
		this.clauseGroupCode = clauseGroupCode;
	}

}
