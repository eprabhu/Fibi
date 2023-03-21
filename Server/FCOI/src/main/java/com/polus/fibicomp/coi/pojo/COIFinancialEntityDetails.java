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
@Table(name = "COI_FIN_ENTITY_DETAILS")
@EntityListeners(AuditingEntityListener.class)
public class COIFinancialEntityDetails implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "COI_FIN_ENTITY_DETAILS_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer financialEntityDetailsId;

	@Column(name = "QUESTIONNAIRE_ANS_HEADER_ID")
	private Integer questionnaireAnsHeaderId;

	@Column(name = "COI_FINANCIAL_ENTITY_ID")
	private Integer coiFinancialEntityId;

	@Column(name = "COI_FIN_ENTITY_REL_TYPE_CODE")
	private String financialEntityRelTypeCode;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "COI_FIN_ENTITY_DETAILS_FK3"), name = "COI_FIN_ENTITY_REL_TYPE_CODE", referencedColumnName = "COI_FIN_ENTITY_REL_TYPE_CODE", insertable = false, updatable = false)
	private COIFinancialEntityRelType coiFinancialEntityRelType;

	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Integer getFinancialEntityDetailsId() {
		return financialEntityDetailsId;
	}

	public void setFinancialEntityDetailsId(Integer financialEntityDetailsId) {
		this.financialEntityDetailsId = financialEntityDetailsId;
	}

	public Integer getQuestionnaireAnsHeaderId() {
		return questionnaireAnsHeaderId;
	}

	public void setQuestionnaireAnsHeaderId(Integer questionnaireAnsHeaderId) {
		this.questionnaireAnsHeaderId = questionnaireAnsHeaderId;
	}

	public Integer getCoiFinancialEntityId() {
		return coiFinancialEntityId;
	}

	public void setCoiFinancialEntityId(Integer coiFinancialEntityId) {
		this.coiFinancialEntityId = coiFinancialEntityId;
	}

	public String getFinancialEntityRelTypeCode() {
		return financialEntityRelTypeCode;
	}

	public void setFinancialEntityRelTypeCode(String financialEntityRelTypeCode) {
		this.financialEntityRelTypeCode = financialEntityRelTypeCode;
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

	public COIFinancialEntityRelType getCoiFinancialEntityRelType() {
		return coiFinancialEntityRelType;
	}

	public void setCoiFinancialEntityRelType(COIFinancialEntityRelType coiFinancialEntityRelType) {
		this.coiFinancialEntityRelType = coiFinancialEntityRelType;
	}

}
