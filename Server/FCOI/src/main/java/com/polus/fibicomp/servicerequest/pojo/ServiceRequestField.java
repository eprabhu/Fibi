package com.polus.fibicomp.servicerequest.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "SR_HEADER_FIELD")
public class ServiceRequestField implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "SR_HEADER_FIELD_ID", length = 12)
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer serviceRequestFieldId;

	@JsonBackReference
	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "SR_HEADER_FIELD_FK1"), name = "SR_HEADER_ID", referencedColumnName = "SR_HEADER_ID")
	private ServiceRequest serviceRequest;

	@Column(name = "FIELD_NAME", length = 30)
	private String fieldName;

	@Column(name = "FIELD_TYPE", length = 30)
	private String fieldType;

	@Column(name = "VALUE", length = 1000)
	private String value;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER", length = 60)
	private String updateUser;

	@Column(name = "IS_EDITABLE")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isEditable = false;

	@Transient
	private Boolean canEmpty = false;

	public Integer getServiceRequestFieldId() {
		return serviceRequestFieldId;
	}

	public void setServiceRequestFieldId(Integer serviceRequestFieldId) {
		this.serviceRequestFieldId = serviceRequestFieldId;
	}

	public ServiceRequest getServiceRequest() {
		return serviceRequest;
	}

	public void setServiceRequest(ServiceRequest serviceRequest) {
		this.serviceRequest = serviceRequest;
	}

	public String getFieldName() {
		return fieldName;
	}

	public void setFieldName(String fieldName) {
		this.fieldName = fieldName;
	}

	public String getValue() {
		return value;
	}

	public void setValue(String value) {
		this.value = value;
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

	public String getFieldType() {
		return fieldType;
	}

	public void setFieldType(String fieldType) {
		this.fieldType = fieldType;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public Boolean getCanEmpty() {
		return canEmpty;
	}

	public void setCanEmpty(Boolean canEmpty) {
		this.canEmpty = canEmpty;
	}

	public Boolean getIsEditable() {
		return isEditable;
	}

	public void setIsEditable(Boolean isEditable) {
		this.isEditable = isEditable;
	}

}
