package com.polus.fibicomp.agreements.pojo;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;

import org.hibernate.annotations.GenericGenerator;

@Entity
@Table(name = "AGREEMENT_NOTE_FILE_DATA")
public class AgreementNoteFileData implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GeneratedValue(generator = "system-uuid")
	@GenericGenerator(name = "system-uuid", strategy = "uuid2")
	@Column(name = "ID", unique = true)
	private String agreementNoteFileDataId;

	@Column(name = "DATA")
	private byte[] fileData;

	public String getAgreementNoteFileDataId() {
		return agreementNoteFileDataId;
	}

	public void setAgreementNoteFileDataId(String agreementNoteFileDataId) {
		this.agreementNoteFileDataId = agreementNoteFileDataId;
	}

	public byte[] getFileData() {
		return fileData;
	}

	public void setFileData(byte[] fileData) {
		this.fileData = fileData;
	}

}
