package com.polus.fibicomp.orcid.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "ORCID_ERROR_LOG")
public class OrcidErrorLog implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "ORCID_ERROR_LOG_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "ORCID_ERROR_LOG_ID_GNRTR")
	@SequenceGenerator(name="ORCID_ERROR_LOG_ID_GNRTR", sequenceName = "ORCID_ERROR_LOG_ID_GNRTR", allocationSize=1)
	private Integer orcidErrorLogId;

	@Column(name = "ORCID_ID")
	private String orcidId;

	@Column(name = "ERROR_MESSAGE")
	private String errorMessage;

	@Column(name = "ERROR_TYPE")
	private String errorType;

	@Column(name = "PUT_CODE")
	private Integer putCode;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "IS_MAIL_SENT")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isMailSent = false;

	public Integer getOrcidErrorLogId() {
		return orcidErrorLogId;
	}

	public void setOrcidErrorLogId(Integer orcidErrorLogId) {
		this.orcidErrorLogId = orcidErrorLogId;
	}

	public String getOrcidId() {
		return orcidId;
	}

	public void setOrcidId(String orcidId) {
		this.orcidId = orcidId;
	}

	public String getErrorMessage() {
		return errorMessage;
	}

	public void setErrorMessage(String errorMessage) {
		this.errorMessage = errorMessage;
	}

	public String getErrorType() {
		return errorType;
	}

	public void setErrorType(String errorType) {
		this.errorType = errorType;
	}

	public Integer getPutCode() {
		return putCode;
	}

	public void setPutCode(Integer putCode) {
		this.putCode = putCode;
	}

	public Timestamp getUpdateTimeStamp() {
		return updateTimeStamp;
	}

	public void setUpdateTimeStamp(Timestamp updateTimeStamp) {
		this.updateTimeStamp = updateTimeStamp;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public Boolean getIsMailSent() {
		return isMailSent;
	}

	public void setIsMailSent(Boolean isMailSent) {
		this.isMailSent = isMailSent;
	}

}
