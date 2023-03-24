package com.polus.fibicomp.negotiation.pojo;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.Table;

import org.hibernate.annotations.GenericGenerator;

@Entity
@Table(name = "NEGOTIATION_COMMENT_FILE_DATA")
public class NegotiationCommentFileData implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GeneratedValue(generator = "system-uuid")
	@GenericGenerator(name = "system-uuid", strategy = "uuid2")
	@Column(name = "ID", unique = true)
	private String negotiationCommentFileDataId;

	@Column(name = "DATA")
	private byte[] fileData;

	public String getNegotiationCommentFileDataId() {
		return negotiationCommentFileDataId;
	}

	public void setNegotiationCommentFileDataId(String negotiationCommentFileDataId) {
		this.negotiationCommentFileDataId = negotiationCommentFileDataId;
	}

	public byte[] getFileData() {
		return fileData;
	}

	public void setFileData(byte[] fileData) {
		this.fileData = fileData;
	}

}
