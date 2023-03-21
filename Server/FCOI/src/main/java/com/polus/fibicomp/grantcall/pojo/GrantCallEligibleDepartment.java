package com.polus.fibicomp.grantcall.pojo;

import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;

import org.hibernate.annotations.GenericGenerator;
import org.hibernate.annotations.Parameter;

@Entity
@Table(name = "GRANT_CALL_ELIGIBLE_DEPARTMENT")
public class GrantCallEligibleDepartment {
	@Id
	@GenericGenerator(name = "grantEligibilityDepartmentIdGenerator", strategy = "increment", parameters = {
			@Parameter(name = "initial_value", value = "1"), @Parameter(name = "increment_size", value = "1") })
	@GeneratedValue(generator = "grantEligibilityDepartmentIdGenerator")
	@Column(name = "ELIGIBILE_DEPT_ID", updatable = false, nullable = false)
	private Integer grantEligibilityDepartmentId;

	@Column(name = "UNIT_NUMBER")
	private String unitNumber;

	@Column(name = "UNIT_NAME")
	private String unitName;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "GRANT_HEADER_ID")
	private Integer grantCallId;

	public Integer getGrantEligibilityDepartmentId() {
		return grantEligibilityDepartmentId;
	}

	public void setGrantEligibilityDepartmentId(Integer grantEligibilityDepartmentId) {
		this.grantEligibilityDepartmentId = grantEligibilityDepartmentId;
	}

	public String getUnitNumber() {
		return unitNumber;
	}

	public void setUnitNumber(String unitNumber) {
		this.unitNumber = unitNumber;
	}

	public String getUnitName() {
		return unitName;
	}

	public void setUnitName(String unitName) {
		this.unitName = unitName;
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

	public Integer getGrantCallId() {
		return grantCallId;
	}

	public void setGrantCallId(Integer grantCallId) {
		this.grantCallId = grantCallId;
	}

}
