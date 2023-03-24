package com.polus.fibicomp.agreements.pojo;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;

import org.hibernate.annotations.GenericGenerator;

@Entity
@Table(name = "AGREEMENT_ATTACHMENT_FILE")
public class AgreementAttachmentFile implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GeneratedValue(generator = "system-uuid")
	@GenericGenerator(name = "system-uuid", strategy = "uuid2")
	@Column(name = "AGREEMENT_ATTACHMENT_FILE_ID", unique = true)
	private String agreementAttachmentFileId;

	@Column(name = "FILEDATA")
	private byte[] fileData;

	public String getAgreementAttachmentFileId() {
		return agreementAttachmentFileId;
	}

	public void setAgreementAttachmentFileId(String agreementAttachmentFileId) {
		this.agreementAttachmentFileId = agreementAttachmentFileId;
	}

	public byte[] getFileData() {
		return fileData;
	}

	public void setFileData(byte[] fileData) {
		this.fileData = fileData;
	}

}
