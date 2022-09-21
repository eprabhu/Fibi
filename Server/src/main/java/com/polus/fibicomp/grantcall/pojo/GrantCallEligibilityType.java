package com.polus.fibicomp.grantcall.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;

import org.hibernate.annotations.GenericGenerator;
import org.hibernate.annotations.Parameter;

import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "GRANT_CALL_ELGIBLITY_TYPE")
public class GrantCallEligibilityType implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GenericGenerator(name = "grantEligibilityTypeCodeGererator", strategy = "increment", parameters = {
			@Parameter(name = "initial_value", value = "1"), @Parameter(name = "increment_size", value = "1") })
	@GeneratedValue(generator = "grantEligibilityTypeCodeGererator")
	@Column(name = "GRANT_ELGBLTY_TYPE_CODE", updatable = false, nullable = false)
	private Integer grantEligibilityTypeCode;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "MORE_INFORMATION")
	private String moreInformation;

	@Column(name = "HAS_TARGET")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean hasTarget;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "IS_ACTIVE")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isActive;

	public Integer getGrantEligibilityTypeCode() {
		return grantEligibilityTypeCode;
	}

	public void setGrantEligibilityTypeCode(Integer grantEligibilityTypeCode) {
		this.grantEligibilityTypeCode = grantEligibilityTypeCode;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public Boolean getHasTarget() {
		return hasTarget;
	}

	public void setHasTarget(Boolean hasTarget) {
		this.hasTarget = hasTarget;
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

	public String getMoreInformation() {
		return moreInformation;
	}

	public void setMoreInformation(String moreInformation) {
		this.moreInformation = moreInformation;
	}

	public Boolean getIsActive() {
		return isActive;
	}

	public void setIsActive(Boolean isActive) {
		this.isActive = isActive;
	}

}
