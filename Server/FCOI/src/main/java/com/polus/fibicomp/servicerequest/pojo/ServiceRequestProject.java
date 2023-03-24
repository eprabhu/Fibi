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

import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import com.polus.fibicomp.pojo.Module;

@Entity
@Table(name = "SR_PROJECT")
@EntityListeners(AuditingEntityListener.class)
public class ServiceRequestProject implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "SR_PROJECT_ID", length = 12)
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer serviceRequestProjectId;

	@Column(name = "SR_HEADER_ID")
	private Integer serviceRequestId;

	@Column(name = "MODULE_CODE")
	private Integer moduleCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "SR_PROJECT_FK2"), name = "MODULE_CODE", referencedColumnName = "MODULE_CODE", insertable = false, updatable = false)
	private Module serviceRequestModule;

	@Column(name = "MODULE_ITEM_KEY")
	private String moduleItemKey;

	@Column(name = "UPDATE_TIMESTAMP")
	@LastModifiedDate
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	@LastModifiedBy
	private String updateUser;

	public Integer getServiceRequestProjectId() {
		return serviceRequestProjectId;
	}

	public void setServiceRequestProjectId(Integer serviceRequestProjectId) {
		this.serviceRequestProjectId = serviceRequestProjectId;
	}

	public Integer getModuleCode() {
		return moduleCode;
	}

	public void setModuleCode(Integer moduleCode) {
		this.moduleCode = moduleCode;
	}

	public String getModuleItemKey() {
		return moduleItemKey;
	}

	public void setModuleItemKey(String moduleItemKey) {
		this.moduleItemKey = moduleItemKey;
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

	public Module getServiceRequestModule() {
		return serviceRequestModule;
	}

	public void setServiceRequestModule(Module serviceRequestModule) {
		this.serviceRequestModule = serviceRequestModule;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public Integer getServiceRequestId() {
		return serviceRequestId;
	}

	public void setServiceRequestId(Integer serviceRequestId) {
		this.serviceRequestId = serviceRequestId;
	}

}
