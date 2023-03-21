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
@Table(name = "SCORING_CRITERIA")
public class ScoringCriteria implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GenericGenerator(name = "scoringCriteriaTypeCodeGenerator", strategy = "increment", parameters = {
			@Parameter(name = "initial_value", value = "1"), @Parameter(name = "increment_size", value = "1") })
	@GeneratedValue(generator = "scoringCriteriaTypeCodeGenerator")
	@Column(name = "SCORING_CRITERIA_TYPE_CODE")
	private String scoringCriteriaTypeCode;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "IS_ACTIVE")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isActive;

	@Column(name = "EXPLANATION")
	private String explanation;

	public ScoringCriteria() {
		super();
	}

	public ScoringCriteria(String scoringCriteriaTypeCode, String description) {
		super();
		this.scoringCriteriaTypeCode = scoringCriteriaTypeCode;
		this.description = description;
	}

	public String getScoringCriteriaTypeCode() {
		return scoringCriteriaTypeCode;
	}

	public void setScoringCriteriaTypeCode(String scoringCriteriaTypeCode) {
		this.scoringCriteriaTypeCode = scoringCriteriaTypeCode;
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

	public Boolean getIsActive() {
		return isActive;
	}

	public void setIsActive(Boolean isActive) {
		this.isActive = isActive;
	}

	public String getExplanation() {
		return explanation;
	}

	public void setExplanation(String explanation) {
		this.explanation = explanation;
	}

}
