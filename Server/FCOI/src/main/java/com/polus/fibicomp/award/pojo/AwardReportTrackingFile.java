package com.polus.fibicomp.award.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

@Entity
@Table(name = "AWARD_REPORT_TRACKING_FILE")
@EntityListeners(AuditingEntityListener.class)
public class AwardReportTrackingFile implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "AWARD_REPORT_TRACKING_FILE_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer awardReportTrackingFileId;

	@Column(name = "AWARD_REPORT_TERMS_ID")
	private Integer awardReportTermsId;

	@Column(name = "AWARD_REPORT_TRACKING_ID")
	private Integer awardReportTrackingId;

	@Column(name = "AWARD_ID")
	private Integer awardId;

	@Column(name = "AWARD_NUMBER")
	private String awardNumber;

	@Column(name = "SEQUENCE_NUMBER")
	private Integer sequenceNumber;

	@Column(name = "FILE_ID")
	private String fileId;

	@Column(name = "VERSION_NUMBER")
	private Integer versionNumber;

	@Column(name = "DOCUMENT_STATUS_CODE")
	private String documentStatusCode;

	@Column(name = "FILE_NAME")
	private String fileName;

	@Column(name = "CONTENT_TYPE")
	private String contentType;

	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;
    
	@Transient
	private String acType;

	@Transient
	private String fullName;

	public Integer getAwardReportTrackingFileId() {
		return awardReportTrackingFileId;
	}

	public void setAwardReportTrackingFileId(Integer awardReportTrackingFileId) {
		this.awardReportTrackingFileId = awardReportTrackingFileId;
	}

	public Integer getAwardReportTermsId() {
		return awardReportTermsId;
	}

	public void setAwardReportTermsId(Integer awardReportTermsId) {
		this.awardReportTermsId = awardReportTermsId;
	}

	public Integer getAwardReportTrackingId() {
		return awardReportTrackingId;
	}

	public void setAwardReportTrackingId(Integer awardReportTrackingId) {
		this.awardReportTrackingId = awardReportTrackingId;
	}

	public Integer getAwardId() {
		return awardId;
	}

	public void setAwardId(Integer awardId) {
		this.awardId = awardId;
	}

	public String getAwardNumber() {
		return awardNumber;
	}

	public void setAwardNumber(String awardNumber) {
		this.awardNumber = awardNumber;
	}

	public String getFileId() {
		return fileId;
	}

	public void setFileId(String fileId) {
		this.fileId = fileId;
	}

	public String getFileName() {
		return fileName;
	}

	public void setFileName(String fileName) {
		this.fileName = fileName;
	}

	public String getContentType() {
		return contentType;
	}

	public void setContentType(String contentType) {
		this.contentType = contentType;
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

	public String getAcType() {
		return acType;
	}

	public void setAcType(String acType) {
		this.acType = acType;
	}

	public Integer getSequenceNumber() {
		return sequenceNumber;
	}

	public void setSequenceNumber(Integer sequenceNumber) {
		this.sequenceNumber = sequenceNumber;
	}

	public Integer getVersionNumber() {
		return versionNumber;
	}

	public void setVersionNumber(Integer versionNumber) {
		this.versionNumber = versionNumber;
	}

	public String getDocumentStatusCode() {
		return documentStatusCode;
	}

	public void setDocumentStatusCode(String documentStatusCode) {
		this.documentStatusCode = documentStatusCode;
	}

	public String getFullName() {
		return fullName;
	}

	public void setFullName(String fullName) {
		this.fullName = fullName;
	}

}
