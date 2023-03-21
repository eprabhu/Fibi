package com.polus.fibicomp.grantcall.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

import com.polus.fibicomp.pojo.SponsorFundingScheme;

@Entity
@Table(name = "FUNDING_SCHEME_ATTACHMENT")
public class FundingSchemeAttachment implements Serializable{

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "FUNDING_SCHEME_ATTACHMENT_ID", updatable = false, nullable = false)
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "FUNDING_SCHEME_ATTACHMENT_ID_GENERATOR")
	@SequenceGenerator(name="FUNDING_SCHEME_ATTACHMENT_ID_GENERATOR", sequenceName = "FUNDING_SCHEME_ATTACHMENT_ID_GENERATOR", allocationSize=1)
	private Integer fundingSchemeAttachmentId;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "FILE_NAME")
	private String fileName;

	@Column(name = "MIME_TYPE")
	private String mimeType;

	@Column(name = "FILE_DATA_ID")
	private String fileDataId;

	@Column(name = "GRANT_ATTACHMNT_TYPE_CODE")
	private Integer grantAttachmentTypeCode;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "FUNDING_SCHEME_ATTACHMENT_FK2"), name = "GRANT_ATTACHMNT_TYPE_CODE", referencedColumnName = "GRANT_ATTACHMNT_TYPE_CODE", insertable = false, updatable = false)
	private GrantCallAttachType grantCallAttachType;

	@Column(name = "FUNDING_SCHEME_ID")
	private Integer fundingSchemeId;

	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "FUNDING_SCHEME_ATTACHMENT_FK1"), name = "FUNDING_SCHEME_ID", referencedColumnName = "FUNDING_SCHEME_ID", insertable = false, updatable = false)
	private SponsorFundingScheme sponsorFundingScheme;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Integer getFundingSchemeAttachmentId() {
		return fundingSchemeAttachmentId;
	}

	public void setFundingSchemeAttachmentId(Integer fundingSchemeAttachmentId) {
		this.fundingSchemeAttachmentId = fundingSchemeAttachmentId;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getFileName() {
		return fileName;
	}

	public void setFileName(String fileName) {
		this.fileName = fileName;
	}

	public String getMimeType() {
		return mimeType;
	}

	public void setMimeType(String mimeType) {
		this.mimeType = mimeType;
	}

	public String getFileDataId() {
		return fileDataId;
	}

	public void setFileDataId(String fileDataId) {
		this.fileDataId = fileDataId;
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

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public Integer getGrantAttachmentTypeCode() {
		return grantAttachmentTypeCode;
	}

	public void setGrantAttachmentTypeCode(Integer grantAttachmentTypeCode) {
		this.grantAttachmentTypeCode = grantAttachmentTypeCode;
	}

	public GrantCallAttachType getGrantCallAttachType() {
		return grantCallAttachType;
	}

	public void setGrantCallAttachType(GrantCallAttachType grantCallAttachType) {
		this.grantCallAttachType = grantCallAttachType;
	}

	public SponsorFundingScheme getSponsorFundingScheme() {
		return sponsorFundingScheme;
	}

	public void setSponsorFundingScheme(SponsorFundingScheme sponsorFundingScheme) {
		this.sponsorFundingScheme = sponsorFundingScheme;
	}

	public Integer getFundingSchemeId() {
		return fundingSchemeId;
	}

	public void setFundingSchemeId(Integer fundingSchemeId) {
		this.fundingSchemeId = fundingSchemeId;
	}

}
