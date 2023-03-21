package com.polus.fibicomp.claims.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "CLAIM_INVOICE_METADATA")
public class ClaimInvoiceMetadata implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "BA_CODE")
	private String baCode;
	
	@Id
	@Column(name = "DOCUMENT_TYPE_CODE")
	private String documentTypeCode;
	
	@Column(name = "DOCUMENT_TYPE_DESC")
	private String documentTypeDesc;
	
	@Column(name = "REVERSAL_DOCUMENT_TYPE_CODE")
	private String reversalDocumentTypeCode;
	
	@Column(name = "REVERSAL_DOCUMENT_TYPE_DESC")
	private String reversalDocumentTypeDesc;
	
	@Column(name = "HEADER_POSTING_KEY")
	private String headerPostingKey;
	
	@Column(name = "LINE_ITEM_POSTING_KEY")
	private String lineItemPostingKey;
	
	@Column(name = "REVERSAL_HEADER_POSTING_KEY")
	private String reversalHeaderPostingKey;
	
	@Column(name = "REVERSAL_LINE_ITEM_POSTING_KEY")
	private String reversalLineItemPostingKey;
	
	@Column(name = "IS_ACTIVE")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isActive;
	
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;
	
	@Column(name = "UPDATE_USER")
	private String updateUser;

	public String getBaCode() {
		return baCode;
	}

	public void setBaCode(String baCode) {
		this.baCode = baCode;
	}

	public String getDocumentTypeCode() {
		return documentTypeCode;
	}

	public void setDocumentTypeCode(String documentTypeCode) {
		this.documentTypeCode = documentTypeCode;
	}

	public String getDocumentTypeDesc() {
		return documentTypeDesc;
	}

	public void setDocumentTypeDesc(String documentTypeDesc) {
		this.documentTypeDesc = documentTypeDesc;
	}

	public String getReversalDocumentTypeCode() {
		return reversalDocumentTypeCode;
	}

	public void setReversalDocumentTypeCode(String reversalDocumentTypeCode) {
		this.reversalDocumentTypeCode = reversalDocumentTypeCode;
	}

	public String getReversalDocumentTypeDesc() {
		return reversalDocumentTypeDesc;
	}

	public void setReversalDocumentTypeDesc(String reversalDocumentTypeDesc) {
		this.reversalDocumentTypeDesc = reversalDocumentTypeDesc;
	}

	public String getHeaderPostingKey() {
		return headerPostingKey;
	}

	public void setHeaderPostingKey(String headerPostingKey) {
		this.headerPostingKey = headerPostingKey;
	}

	public String getReversalHeaderPostingKey() {
		return reversalHeaderPostingKey;
	}

	public void setReversalHeaderPostingKey(String reversalHeaderPostingKey) {
		this.reversalHeaderPostingKey = reversalHeaderPostingKey;
	}

	public String getReversalLineItemPostingKey() {
		return reversalLineItemPostingKey;
	}

	public void setReversalLineItemPostingKey(String reversalLineItemPostingKey) {
		this.reversalLineItemPostingKey = reversalLineItemPostingKey;
	}

	public Boolean getIsActive() {
		return isActive;
	}

	public void setIsActive(Boolean isActive) {
		this.isActive = isActive;
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

	public String getLineItemPostingKey() {
		return lineItemPostingKey;
	}

	public void setLineItemPostingKey(String lineItemPostingKey) {
		this.lineItemPostingKey = lineItemPostingKey;
	}
}
