package com.polus.fibicomp.servicerequest.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Convert;
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

import org.springframework.data.annotation.CreatedBy;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import com.polus.fibicomp.agreements.pojo.AdminGroup;
import com.polus.fibicomp.award.awardprojectoutcome.dto.ModuleDetails;
import com.polus.fibicomp.pojo.Module;
import com.polus.fibicomp.pojo.Unit;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "SR_HEADER")
@EntityListeners(AuditingEntityListener.class)
public class ServiceRequest implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "SR_HEADER_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer serviceRequestId;

	@Column(name = "STATUS_CODE")
	private Integer statusCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "SR_HEADER_FK2"), name = "STATUS_CODE", referencedColumnName = "STATUS_CODE", insertable = false, updatable = false)
	private ServiceRequestStatus serviceRequestStatus;

	@Column(name = "TYPE_CODE")
	private String typeCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "SR_HEADER_FK1"), name = "TYPE_CODE", referencedColumnName = "TYPE_CODE", insertable = false, updatable = false)
	private ServiceRequestType serviceRequestType;

	@Column(name = "MODULE_CODE")
	private Integer moduleCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "SR_HEADER_FK3"), name = "MODULE_CODE", referencedColumnName = "MODULE_CODE", insertable = false, updatable = false)
	private Module serviceRequestModule;

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

	@Column(name = "UNIT_NUMBER")
	private String unitNumber;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "SR_HEADER_FK5"), name = "UNIT_NUMBER", referencedColumnName = "UNIT_NUMBER", insertable = false, updatable = false)
	private Unit unit;

	@Column(name = "ORIGINATING_MODULE_ITEM_KEY")
	private String originatingModuleItemKey;

	@Column(name = "CREATE_TIMESTAMP")
	@CreatedDate
	private Timestamp createTimestamp;

	@Column(name = "CREATE_USER")
	@CreatedBy
	private String createUser;

	@Column(name = "UPDATE_TIMESTAMP")
	@LastModifiedDate
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	@LastModifiedBy
	private String updateUser;

	@Column(name = "IS_SYSTEM_GENERATED")
	@Convert (converter = JpaCharBooleanConversion.class)
	private Boolean isSystemGenerated;

	@Column(name = "PRIORITY_ID")
	private Integer priorityId;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "SR_HEADER_FK4"), name = "PRIORITY_ID", referencedColumnName = "PRIORITY_ID", insertable = false, updatable = false)
	private ServiceRequestPriority serviceRequestPriority;

	@Column(name = "ADMIN_GROUP_ID")
	private Integer adminGroupId;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "SR_HEADER_FK5"), name = "ADMIN_GROUP_ID", referencedColumnName = "ADMIN_GROUP_ID", insertable = false, updatable = false)
	private AdminGroup adminGroup;

	@Transient
	private String createUserFullName;

	@Transient
	private String updateUserFullName;

	@Transient
	private String submitUserFullName;

	@Transient
	private String createdPersonName;

	@Transient
	private String serviceRequestTypeData;

	@Transient
	private String serviceRequestStatusData;

	@Transient
	private String unitName;

	@Transient
	private String reporterPersonName;

	@Transient
	private String assigneePersonName;

	@Transient
	private String sRPriority;

	@Transient
	private String serviceRequestCategory;

	@Transient
	private ModuleDetails moduleDetails;

	public ServiceRequest() {
		super();
	}

	public ServiceRequest(Integer serviceRequestId, String subject, String description, String originatingModuleItemKey) {
		super();
		this.serviceRequestId = serviceRequestId;
		this.subject = subject;
		this.description = description;
		this.originatingModuleItemKey = originatingModuleItemKey;
	}

	public Integer getServiceRequestId() {
		return serviceRequestId;
	}

	public void setServiceRequestId(Integer serviceRequestId) {
		this.serviceRequestId = serviceRequestId;
	}

	public Integer getStatusCode() {
		return statusCode;
	}

	public void setStatusCode(Integer statusCode) {
		this.statusCode = statusCode;
	}

	public ServiceRequestStatus getServiceRequestStatus() {
		return serviceRequestStatus;
	}

	public void setServiceRequestStatus(ServiceRequestStatus serviceRequestStatus) {
		this.serviceRequestStatus = serviceRequestStatus;
	}

	public String getTypeCode() {
		return typeCode;
	}

	public void setTypeCode(String typeCode) {
		this.typeCode = typeCode;
	}

	public ServiceRequestType getServiceRequestType() {
		return serviceRequestType;
	}

	public void setServiceRequestType(ServiceRequestType serviceRequestType) {
		this.serviceRequestType = serviceRequestType;
	}

	public Integer getModuleCode() {
		return moduleCode;
	}

	public void setModuleCode(Integer moduleCode) {
		this.moduleCode = moduleCode;
	}

	public Module getServiceRequestModule() {
		return serviceRequestModule;
	}

	public void setServiceRequestModule(Module serviceRequestModule) {
		this.serviceRequestModule = serviceRequestModule;
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

	public String getUnitNumber() {
		return unitNumber;
	}

	public void setUnitNumber(String unitNumber) {
		this.unitNumber = unitNumber;
	}

	public String getOriginatingModuleItemKey() {
		return originatingModuleItemKey;
	}

	public void setOriginatingModuleItemKey(String originatingModuleItemKey) {
		this.originatingModuleItemKey = originatingModuleItemKey;
	}

	public Timestamp getCreateTimestamp() {
		return createTimestamp;
	}

	public void setCreateTimestamp(Timestamp createTimestamp) {
		this.createTimestamp = createTimestamp;
	}

	public String getCreateUser() {
		return createUser;
	}

	public void setCreateUser(String createUser) {
		this.createUser = createUser;
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

	public Boolean getIsSystemGenerated() {
		return isSystemGenerated;
	}

	public void setIsSystemGenerated(Boolean isSystemGenerated) {
		this.isSystemGenerated = isSystemGenerated;
	}

	public String getCreateUserFullName() {
		return createUserFullName;
	}

	public void setCreateUserFullName(String createUserFullName) {
		this.createUserFullName = createUserFullName;
	}

	public String getUpdateUserFullName() {
		return updateUserFullName;
	}

	public void setUpdateUserFullName(String updateUserFullName) {
		this.updateUserFullName = updateUserFullName;
	}

	public String getSubmitUserFullName() {
		return submitUserFullName;
	}

	public void setSubmitUserFullName(String submitUserFullName) {
		this.submitUserFullName = submitUserFullName;
	}

	public String getCreatedPersonName() {
		return createdPersonName;
	}

	public void setCreatedPersonName(String createdPersonName) {
		this.createdPersonName = createdPersonName;
	}

	public String getServiceRequestTypeData() {
		return serviceRequestTypeData;
	}

	public void setServiceRequestTypeData(String serviceRequestTypeData) {
		this.serviceRequestTypeData = serviceRequestTypeData;
	}

	public String getServiceRequestStatusData() {
		return serviceRequestStatusData;
	}

	public void setServiceRequestStatusData(String serviceRequestStatusData) {
		this.serviceRequestStatusData = serviceRequestStatusData;
	}

	public ServiceRequestPriority getServiceRequestPriority() {
		return serviceRequestPriority;
	}

	public void setServiceRequestPriority(ServiceRequestPriority serviceRequestPriority) {
		this.serviceRequestPriority = serviceRequestPriority;
	}

	public Unit getUnit() {
		return unit;
	}

	public void setUnit(Unit unit) {
		this.unit = unit;
	}

	public String getUnitName() {
		return unitName;
	}

	public void setUnitName(String unitName) {
		this.unitName = unitName;
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

	public AdminGroup getAdminGroup() {
		return adminGroup;
	}

	public void setAdminGroup(AdminGroup adminGroup) {
		this.adminGroup = adminGroup;
	}

	public Integer getAdminGroupId() {
		return adminGroupId;
	}

	public void setAdminGroupId(Integer adminGroupId) {
		this.adminGroupId = adminGroupId;
	}

	public String getsRPriority() {
		return sRPriority;
	}

	public void setsRPriority(String sRPriority) {
		this.sRPriority = sRPriority;
	}

	public String getServiceRequestCategory() {
		return serviceRequestCategory;
	}

	public void setServiceRequestCategory(String serviceRequestCategory) {
		this.serviceRequestCategory = serviceRequestCategory;
	}

	public Integer getPriorityId() {
		return priorityId;
	}

	public void setPriorityId(Integer priorityId) {
		this.priorityId = priorityId;
	}

	public ModuleDetails getModuleDetails() {
		return moduleDetails;
	}

	public void setModuleDetails(ModuleDetails moduleDetails) {
		this.moduleDetails = moduleDetails;
	}

}
