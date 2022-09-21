package com.polus.fibicomp.agreements.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.CascadeType;
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

import com.fasterxml.jackson.annotation.JsonBackReference;

@Entity
@Table(name = "CLAUSES")
public class Clauses implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SEQ_CLAUSES")
	@SequenceGenerator(name="SEQ_CLAUSES", sequenceName = "SEQ_CLAUSES", allocationSize=1)
	@Column(name = "CLAUSES_CODE")
	private Integer clauseCode;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "IS_SYSTEM_SPECIFIC")
	private String isSystemSpecific;

	@Column(name = "IS_ACTIVE")
	private String isActive;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@JsonBackReference
	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "CLAUSES_FK1"), name = "CLAUSES_GROUP_CODE", referencedColumnName = "CLAUSES_GROUP_CODE")
	private ClausesGroup clausesGroup;

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getIsSystemSpecific() {
		return isSystemSpecific;
	}

	public void setIsSystemSpecific(String isSystemSpecific) {
		this.isSystemSpecific = isSystemSpecific;
	}

	public String getIsActive() {
		return isActive;
	}

	public void setIsActive(String isActive) {
		this.isActive = isActive;
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

	public void setClausesGroup(ClausesGroup clausesGroup) {
		this.clausesGroup = clausesGroup;
	}

	public Integer getClauseCode() {
		return clauseCode;
	}

	public void setClauseCode(Integer clauseCode) {
		this.clauseCode = clauseCode;
	}

}
