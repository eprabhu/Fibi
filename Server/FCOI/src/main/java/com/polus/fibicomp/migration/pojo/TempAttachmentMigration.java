package com.polus.fibicomp.migration.pojo;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

@Entity
@Table(name = "TEMP_ATTACHMENT_MIGRATION")
public class TempAttachmentMigration implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "ATTACHMENT_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "FIBI_MIGRATION_ATTACHMENT_ID_GENERATOR")
	@SequenceGenerator(name = "FIBI_MIGRATION_ATTACHMENT_ID_GENERATOR", sequenceName = "FIBI_MIGRATION_ATTACHMENT_ID_GENERATOR", allocationSize = 1)
	private Integer id;

	@Column(name = "PROJECT_ID")
	private String projectId;
	
	@Column(name = "FINANCE_PROJECT_ID")
	private String financeProjectId;

	@Column(name = "PROJECT_TYPE")
	private String projectType;

	@Column(name = "ATTACHMENT_TYPE")
	private String attachmentType;

	@Column(name = "FILE_NAME")
	private String fileName;

	@Column(name = "MIME_TYPE")
	private String mimeType;

	@Column(name = "DATA")
	private byte[] attachment;

	public String getProjectId() {
		return projectId;
	}

	public void setProjectId(String projectId) {
		this.projectId = projectId;
	}

	public String getProjectType() {
		return projectType;
	}

	public void setProjectType(String projectType) {
		this.projectType = projectType;
	}

	public String getAttachmentType() {
		return attachmentType;
	}

	public void setAttachmentType(String attachmentType) {
		this.attachmentType = attachmentType;
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

	public byte[] getAttachment() {
		return attachment;
	}

	public void setAttachment(byte[] attachment) {
		this.attachment = attachment;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public Integer getId() {
		return id;
	}

	public void setId(Integer id) {
		this.id = id;
	}

	public String getFinanceProjectId() {
		return financeProjectId;
	}

	public void setFinanceProjectId(String financeProjectId) {
		this.financeProjectId = financeProjectId;
	}

}
