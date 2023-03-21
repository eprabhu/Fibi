package com.polus.fibicomp.agreements.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "AGREEMENT_ATTACHMENT_STATUS")
public class AgreementAttachmentStatus implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "AGRMNT_ATTACH_STATUS_CODE")
	private String code;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "IS_ACTIVE")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isActive;

	@Override
	public boolean equals(Object obj) {
		if (obj == this)
			return true;
		if (obj instanceof AgreementAttachmentStatus) {
			AgreementAttachmentStatus other = (AgreementAttachmentStatus) obj;
			return StringUtils.equals(this.code, other.code) && StringUtils.equals(this.description, other.description);
		}
		return false;
	}

	  @Override
	  public int hashCode() {
		  return new HashCodeBuilder(17, 37).append(this.code).append(this.description).toHashCode();
	  }

	public String getCode() {
		return code;
	}

	public void setCode(String code) {
		this.code = code;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
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
