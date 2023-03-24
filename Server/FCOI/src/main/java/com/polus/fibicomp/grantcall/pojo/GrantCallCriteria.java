package com.polus.fibicomp.grantcall.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

@Entity
@Table(name = "GRANT_CALL_CRITERIA")
public class GrantCallCriteria implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "GRANT_CRITERIA_CODE", updatable = false, nullable = false)
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "GRANT_CRITERIA_CODE_GENERATOR")
	@SequenceGenerator(name="GRANT_CRITERIA_CODE_GENERATOR", sequenceName = "GRANT_CRITERIA_CODE_GENERATOR", allocationSize=1)
	private Integer grantCriteriaCode;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public GrantCallCriteria() {
		super();
	}

	public GrantCallCriteria(Integer grantCriteriaCode, String description) {
		super();
		this.grantCriteriaCode = grantCriteriaCode;
		this.description = description;
	}

	public Integer getGrantCriteriaCode() {
		return grantCriteriaCode;
	}

	public void setGrantCriteriaCode(Integer grantCriteriaCode) {
		this.grantCriteriaCode = grantCriteriaCode;
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

	public static long getSerialversionuid() {
		return serialVersionUID;
	}
}
