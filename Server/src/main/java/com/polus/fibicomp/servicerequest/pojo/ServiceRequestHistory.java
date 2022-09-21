package com.polus.fibicomp.servicerequest.pojo;

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
import javax.persistence.Transient;

import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import com.polus.fibicomp.pojo.Module;

@Entity
@Table(name = "SR_HEADER_HISTORY")
@EntityListeners(AuditingEntityListener.class)
public class ServiceRequestHistory implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "SR_HEADER_HISTORY_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer serviceRequestHistoryId;

	@Column(name = "SR_HEADER_ID")
	private Integer serviceRequestId;

	@Column(name = "ACTION_LOG_ID")
	private Integer actionLogId;

	@Column(name = "TYPE_CODE")
	private String typeCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "SR_HEADER_HISTORY_FK3"), name = "TYPE_CODE", referencedColumnName = "TYPE_CODE", insertable = false, updatable = false)
	private ServiceRequestType serviceRequestType;

	@Column(name = "PREV_TYPE_CODE")
	private String previousTypeCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "SR_HEADER_HISTORY_FK4"), name = "PREV_TYPE_CODE", referencedColumnName = "TYPE_CODE", insertable = false, updatable = false)
	private ServiceRequestType previousServiceRequestType;

	@Column(name = "MODULE_CODE")
	private Integer moduleCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "SR_HEADER_HISTORY_FK5"), name = "MODULE_CODE", referencedColumnName = "MODULE_CODE", insertable = false, updatable = false)
	private Module requestModule;

	@Column(name = "PREV_MODULE_CODE")
	private Integer previousModuleCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "SR_HEADER_HISTORY_FK6"), name = "PREV_MODULE_CODE", referencedColumnName = "MODULE_CODE", insertable = false, updatable = false)
	private Module previousRequestModule;

	@Column(name = "MODULE_ITEM_KEY")
	private String moduleItemKey;

	@Column(name = "SUBJECT")
	private String subject;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "REPORTER_PERSON_ID")
	private String reporterPersonId;

	@Column(name = "ASSIGNEE_PERSON_ID")
	private String assigneePersonId;

	@Column(name = "UPDATE_TIMESTAMP")
	@LastModifiedDate
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	@LastModifiedBy
	private String updateUser;

	@Column(name = "PREV_SUBJECT")
	private String previousSubject;

	@Column(name = "PREV_DESCRIPTION")
	private String previousDescription;

	@Column(name = "PREV_ASSIGNEE_PERSON_ID")
	private String previousAssigneePersonId;

	@Column(name = "PREV_MODULE_ITEM_KEY")
	private String previousModuleItemKey;

	@Column(name = "PRIORITY_ID")
	private Integer priorityId;

	@Column(name = "ADMIN_GROUP_ID")
	private Integer adminGroupId;

	@Column(name = "PREV_PRIORITY_ID")
	private Integer previousPriorityId;

	@Column(name = "PREV_ADMIN_GROUP_ID")
	private Integer previousAdminGroupId;

	@Column(name = "UNIT_NUMBER")
	private String unitNumber;

	@Column(name = "PREV_UNIT_NUMBER")
	private String previousUnitNumber;

	@Column(name = "PREV_REPORTER_PERSON_ID")
	private String previousReporterPersonId;

	@Transient
	private String srType;

	@Transient
	private String previousSrType;

	@Transient
	private String unitName;

	@Transient
	private String previousUnitName;

	@Transient
	private String reporterPersonName;

	@Transient
	private String assigneePersonName;

	@Transient
	private String sRPriority;

	@Transient
	private String  previousReporterPersonName;

	@Transient
	private String  previousAssigneePersonName;

	@Transient
	private String  previousSrPriority;

	@Transient
	private String adminGroupName;

	@Transient
	private String previousAdminGroupName;

	@Transient
	private String moduleCodeDescription;

	@Transient
	private String previousModuleCodeDescription;

	@Transient
	private String title;

	@Transient
	private String previousTitle;

	public Integer getServiceRequestHistoryId() {
		return serviceRequestHistoryId;
	}

	public void setServiceRequestHistoryId(Integer serviceRequestHistoryId) {
		this.serviceRequestHistoryId = serviceRequestHistoryId;
	}

	public Integer getModuleCode() {
		return moduleCode;
	}

	public void setModuleCode(Integer moduleCode) {
		this.moduleCode = moduleCode;
	}

	public Integer getPreviousModuleCode() {
		return previousModuleCode;
	}

	public void setPreviousModuleCode(Integer previousModuleCode) {
		this.previousModuleCode = previousModuleCode;
	}

	public String getModuleItemKey() {
		return moduleItemKey;
	}

	public void setModuleItemKey(String moduleItemKey) {
		this.moduleItemKey = moduleItemKey;
	}

	public String getSubject() {
		return subject;
	}

	public void setSubject(String subject) {
		this.subject = subject;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getReporterPersonId() {
		return reporterPersonId;
	}

	public void setReporterPersonId(String reporterPersonId) {
		this.reporterPersonId = reporterPersonId;
	}

	public String getAssigneePersonId() {
		return assigneePersonId;
	}

	public void setAssigneePersonId(String assigneePersonId) {
		this.assigneePersonId = assigneePersonId;
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

	public String getPreviousSubject() {
		return previousSubject;
	}

	public void setPreviousSubject(String previousSubject) {
		this.previousSubject = previousSubject;
	}

	public String getPreviousDescription() {
		return previousDescription;
	}

	public void setPreviousDescription(String previousDescription) {
		this.previousDescription = previousDescription;
	}

	public String getPreviousAssigneePersonId() {
		return previousAssigneePersonId;
	}

	public void setPreviousAssigneePersonId(String previousAssigneePersonId) {
		this.previousAssigneePersonId = previousAssigneePersonId;
	}

	public String getPreviousModuleItemKey() {
		return previousModuleItemKey;
	}

	public void setPreviousModuleItemKey(String previousModuleItemKey) {
		this.previousModuleItemKey = previousModuleItemKey;
	}

	public ServiceRequestType getServiceRequestType() {
		return serviceRequestType;
	}

	public void setServiceRequestType(ServiceRequestType serviceRequestType) {
		this.serviceRequestType = serviceRequestType;
	}

	public Integer getActionLogId() {
		return actionLogId;
	}

	public void setActionLogId(Integer actionLogId) {
		this.actionLogId = actionLogId;
	}

	public Module getRequestModule() {
		return requestModule;
	}

	public void setRequestModule(Module requestModule) {
		this.requestModule = requestModule;
	}

	public Module getPreviousRequestModule() {
		return previousRequestModule;
	}

	public void setPreviousRequestModule(Module previousRequestModule) {
		this.previousRequestModule = previousRequestModule;
	}

	public ServiceRequestType getPreviousServiceRequestType() {
		return previousServiceRequestType;
	}

	public void setPreviousServiceRequestType(ServiceRequestType previousServiceRequestType) {
		this.previousServiceRequestType = previousServiceRequestType;
	}

	public String getTypeCode() {
		return typeCode;
	}

	public void setTypeCode(String typeCode) {
		this.typeCode = typeCode;
	}

	public String getPreviousTypeCode() {
		return previousTypeCode;
	}

	public void setPreviousTypeCode(String previousTypeCode) {
		this.previousTypeCode = previousTypeCode;
	}

	public Integer getServiceRequestId() {
		return serviceRequestId;
	}

	public void setServiceRequestId(Integer serviceRequestId) {
		this.serviceRequestId = serviceRequestId;
	}

	public Integer getPriorityId() {
		return priorityId;
	}

	public void setPriorityId(Integer priorityId) {
		this.priorityId = priorityId;
	}

	public Integer getAdminGroupId() {
		return adminGroupId;
	}

	public void setAdminGroupId(Integer adminGroupId) {
		this.adminGroupId = adminGroupId;
	}

	public Integer getPreviousPriorityId() {
		return previousPriorityId;
	}

	public void setPreviousPriorityId(Integer previousPriorityId) {
		this.previousPriorityId = previousPriorityId;
	}

	public Integer getPreviousAdminGroupId() {
		return previousAdminGroupId;
	}

	public void setPreviousAdminGroupId(Integer previousAdminGroupId) {
		this.previousAdminGroupId = previousAdminGroupId;
	}

	public String getPreviousReporterPersonId() {
		return previousReporterPersonId;
	}

	public void setPreviousReporterPersonId(String previousReporterPersonId) {
		this.previousReporterPersonId = previousReporterPersonId;
	}

	public String getUnitNumber() {
		return unitNumber;
	}

	public void setUnitNumber(String unitNumber) {
		this.unitNumber = unitNumber;
	}

	public String getPreviousUnitNumber() {
		return previousUnitNumber;
	}

	public void setPreviousUnitNumber(String previousUnitNumber) {
		this.previousUnitNumber = previousUnitNumber;
	}

	public String getSrType() {
		return srType;
	}

	public void setSrType(String srType) {
		this.srType = srType;
	}

	public String getPreviousSrType() {
		return previousSrType;
	}

	public void setPreviousSrType(String previousSrType) {
		this.previousSrType = previousSrType;
	}

	public String getUnitName() {
		return unitName;
	}

	public void setUnitName(String unitName) {
		this.unitName = unitName;
	}

	public String getPreviousUnitName() {
		return previousUnitName;
	}

	public void setPreviousUnitName(String previousUnitName) {
		this.previousUnitName = previousUnitName;
	}

	public String getReporterPersonName() {
		return reporterPersonName;
	}

	public void setReporterPersonName(String reporterPersonName) {
		this.reporterPersonName = reporterPersonName;
	}

	public String getAssigneePersonName() {
		return assigneePersonName;
	}

	public void setAssigneePersonName(String assigneePersonName) {
		this.assigneePersonName = assigneePersonName;
	}

	public String getsRPriority() {
		return sRPriority;
	}

	public void setsRPriority(String sRPriority) {
		this.sRPriority = sRPriority;
	}

	public String getPreviousReporterPersonName() {
		return previousReporterPersonName;
	}

	public void setPreviousReporterPersonName(String previousReporterPersonName) {
		this.previousReporterPersonName = previousReporterPersonName;
	}

	public String getPreviousAssigneePersonName() {
		return previousAssigneePersonName;
	}

	public void setPreviousAssigneePersonName(String previousAssigneePersonName) {
		this.previousAssigneePersonName = previousAssigneePersonName;
	}

	public String getPreviousSrPriority() {
		return previousSrPriority;
	}

	public void setPreviousSrPriority(String previousSrPriority) {
		this.previousSrPriority = previousSrPriority;
	}

	public String getAdminGroupName() {
		return adminGroupName;
	}

	public void setAdminGroupName(String adminGroupName) {
		this.adminGroupName = adminGroupName;
	}

	public String getPreviousAdminGroupName() {
		return previousAdminGroupName;
	}

	public void setPreviousAdminGroupName(String previousAdminGroupName) {
		this.previousAdminGroupName = previousAdminGroupName;
	}

	public String getModuleCodeDescription() {
		return moduleCodeDescription;
	}

	public void setModuleCodeDescription(String moduleCodeDescription) {
		this.moduleCodeDescription = moduleCodeDescription;
	}

	public String getPreviousModuleCodeDescription() {
		return previousModuleCodeDescription;
	}

	public void setPreviousModuleCodeDescription(String previousModuleCodeDescription) {
		this.previousModuleCodeDescription = previousModuleCodeDescription;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public String getPreviousTitle() {
		return previousTitle;
	}

	public void setPreviousTitle(String previousTitle) {
		this.previousTitle = previousTitle;
	}

}
