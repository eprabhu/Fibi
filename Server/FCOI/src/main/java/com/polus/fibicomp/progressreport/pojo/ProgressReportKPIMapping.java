package com.polus.fibicomp.progressreport.pojo;

import java.io.Serializable;
import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "PROGRESS_REPORT_KPI_MAPPING")
public class ProgressReportKPIMapping implements Serializable {

	private static final long serialVersionUID = 1L;
	
	@Id
	@Column(name = "KPI_CRITERIA_TYPE_CODE")
	private String kpiCriteriaTypeCode;
	
	@Column(name = "KPI_TYPE_CODE")
	private String kpiCategoryTypeCode;	
	
	@Column(name = "TEMPLATE_TABLE")
	private String templateTable;
	
	@Column(name = "SECTION_CODE")
	private String sectionCode;
	
	@JsonIgnore
	@Column(name = "UPDATE_TIMESTAMP")
	private Date updateTimestamp;

	@JsonIgnore
	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "IS_ACTIVE")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isActive;

	public String getKpiCategoryTypeCode() {
		return kpiCategoryTypeCode;
	}

	public void setKpiCategoryTypeCode(String kpiCategoryTypeCode) {
		this.kpiCategoryTypeCode = kpiCategoryTypeCode;
	}

	public String getKpiCriteriaTypeCode() {
		return kpiCriteriaTypeCode;
	}

	public void setKpiCriteriaTypeCode(String kpiCriteriaTypeCode) {
		this.kpiCriteriaTypeCode = kpiCriteriaTypeCode;
	}

	public String getTemplateTable() {
		return templateTable;
	}

	public void setTemplateTable(String templateTable) {
		this.templateTable = templateTable;
	}

	public String getSectionCode() {
		return sectionCode;
	}

	public void setSectionCode(String sectionCode) {
		this.sectionCode = sectionCode;
	}

	public Date getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Date updateTimestamp) {
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
