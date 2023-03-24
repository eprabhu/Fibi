package com.polus.fibicomp.currentandpending.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.OneToMany;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonManagedReference;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "CP_REPORT_HEADER")
public class CPReportHeader implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "CP_REPORT_HEADER_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "CP_REPORT_HEADER_ID_GENERATOR")
	@SequenceGenerator(name = "CP_REPORT_HEADER_ID_GENERATOR", sequenceName = "CP_REPORT_HEADER_ID_GENERATOR", allocationSize = 1)
	private Integer cpReportHeaderId;

	@Column(name = "PERSON_ID")
	private String personId;

	@Column(name = "NON_EMPLOYEE_FLAG")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean nonEmployeeFlag;

	@Column(name = "VERSION_NUMBER")
	private Integer versionNumber;

	@Column(name = "MODULE_CODE")
	private Integer moduleCode;

	@Column(name = "MODULE_ITEM_ID")
	private String moduleItemId;

	@Column(name = "OVERLAP")
	private String overlap;

	@Column(name = "FOOTNOTE")
	private String footNote;

	@Column(name = "CREATE_USER")
	private String createUser;

	@Column(name = "CREATE_TIMESTAMP")
	private Timestamp createTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@JsonManagedReference
	@OneToMany(mappedBy = "cpReportHeader", orphanRemoval = true, cascade = { CascadeType.ALL }, fetch = FetchType.LAZY)
	private List<CPReportProjectDetail> cpReportProjectDetails;

	public Integer getCpReportHeaderId() {
		return cpReportHeaderId;
	}

	public void setCpReportHeaderId(Integer cpReportHeaderId) {
		this.cpReportHeaderId = cpReportHeaderId;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public Boolean getNonEmployeeFlag() {
		return nonEmployeeFlag;
	}

	public void setNonEmployeeFlag(Boolean nonEmployeeFlag) {
		this.nonEmployeeFlag = nonEmployeeFlag;
	}

	public Integer getVersionNumber() {
		return versionNumber;
	}

	public void setVersionNumber(Integer versionNumber) {
		this.versionNumber = versionNumber;
	}

	public Integer getModuleCode() {
		return moduleCode;
	}

	public void setModuleCode(Integer moduleCode) {
		this.moduleCode = moduleCode;
	}

	public String getModuleItemId() {
		return moduleItemId;
	}

	public void setModuleItemId(String moduleItemId) {
		this.moduleItemId = moduleItemId;
	}

	public String getOverlap() {
		return overlap;
	}

	public void setOverlap(String overlap) {
		this.overlap = overlap;
	}

	public String getFootNote() {
		return footNote;
	}

	public void setFootNote(String footNote) {
		this.footNote = footNote;
	}

	public String getCreateUser() {
		return createUser;
	}

	public void setCreateUser(String createUser) {
		this.createUser = createUser;
	}

	public Timestamp getCreateTimestamp() {
		return createTimestamp;
	}

	public void setCreateTimestamp(Timestamp createTimestamp) {
		this.createTimestamp = createTimestamp;
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

	public List<CPReportProjectDetail> getCpReportProjectDetails() {
		return cpReportProjectDetails;
	}

	public void setCpReportProjectDetails(List<CPReportProjectDetail> cpReportProjectDetails) {
		this.cpReportProjectDetails = cpReportProjectDetails;
	}

}
