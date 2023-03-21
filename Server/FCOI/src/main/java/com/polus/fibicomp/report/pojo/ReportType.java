package com.polus.fibicomp.report.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.Id;
import javax.persistence.OneToMany;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonManagedReference;

@Entity
@Table(name= "REPORT_TYPE")
public class ReportType implements Serializable{

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "TYPE_CODE")
	private String typeCode;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "VIEW_NAME")
	private String viewName;

	@Column(name = "JSON_NAME")
	private String jsonName;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "IS_ACTIVE")
	private String isActive;

	@Column(name = "TYPE")
	private String type;

	@JsonManagedReference
	@OneToMany(mappedBy = "reportType", orphanRemoval = true, cascade = { CascadeType.REMOVE, CascadeType.ALL }, fetch = FetchType.EAGER)
	private List<BirtDownloadOption> birtDownloadOptions;

	public String getTypeCode() {
		return typeCode;
	}

	public void setTypeCode(String typeCode) {
		this.typeCode = typeCode;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getViewName() {
		return viewName;
	}

	public void setViewName(String viewName) {
		this.viewName = viewName;
	}

	public String getJsonName() {
		return jsonName;
	}

	public void setJsonName(String jsonName) {
		this.jsonName = jsonName;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

	public String getIsActive() {
		return isActive;
	}

	public void setIsActive(String isActive) {
		this.isActive = isActive;
	}

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public List<BirtDownloadOption> getBirtDownloadOptions() {
		return birtDownloadOptions;
	}

	public void setBirtDownloadOptions(List<BirtDownloadOption> birtDownloadOptions) {
		this.birtDownloadOptions = birtDownloadOptions;
	}
}
