package com.polus.fibicomp.coi.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "COI_FIN_ENTITY_REL_TYPE")
public class COIFinancialEntityRelType implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "COI_FIN_ENTITY_REL_TYPE_CODE")
	private String financialEntityRelTypeCode;

	@Column(name = "DESCRIPTION")
	private String description;
	
	@Column(name = "QUESTIONNAIRE_NUMBER")
	private Integer questionnaireNumber;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "IS_ACTIVE")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isActive;

	public String getFinancialEntityRelTypeCode() {
		return financialEntityRelTypeCode;
	}

	public void setFinancialEntityRelTypeCode(String financialEntityRelTypeCode) {
		this.financialEntityRelTypeCode = financialEntityRelTypeCode;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public Integer getQuestionnaireNumber() {
		return questionnaireNumber;
	}

	public void setQuestionnaireNumber(Integer questionnaireNumber) {
		this.questionnaireNumber = questionnaireNumber;
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

	public Boolean getIsActive() {
		return isActive;
	}

	public void setIsActive(Boolean isActive) {
		this.isActive = isActive;
	}

}
