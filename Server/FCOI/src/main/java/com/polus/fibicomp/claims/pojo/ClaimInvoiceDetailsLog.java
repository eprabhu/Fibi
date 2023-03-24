package com.polus.fibicomp.claims.pojo;

import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import com.fasterxml.jackson.annotation.JsonBackReference;

@Entity
@Table(name = "CLAIM_INVOICE_DETAILS_LOG")
@EntityListeners(AuditingEntityListener.class)
public class ClaimInvoiceDetailsLog implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SEQ_CLAIM_INVOICE_DETAILS_LOG")
	@SequenceGenerator(name = "SEQ_CLAIM_INVOICE_DETAILS_LOG", sequenceName = "SEQ_CLAIM_INVOICE_DETAILS_LOG", allocationSize = 1)
	@Column(name = "INVOICE_DETAIL_ID")
	private Integer invoiceDetailId;
	
	@Column(name = "INVOICE_ID")
	private Integer invoiceId;

	@Column(name = "CLAIM_INVOICE_LOG_ID")
	private Integer claimInvoiceLogId;

	@Column(name = "CLAIM_ID")
	private Integer claimId;
	
	@Column(name = "LINE_ITEM_POSTING_KEY")
	private String lineItemPostingKey;

	@Column(name = "GL_ACCOUNT_CODE")
	private String glAccountCode;

	@Column(name = "CLAIM_AMOUNT")
	private BigDecimal claimAmount = BigDecimal.ZERO;
	
	@Column(name = "SUB_CONTRACT_AMOUNT")
	private BigDecimal subContractAmount = BigDecimal.ZERO;

	@Column(name = "GRT_WBS")
	private String grtWbs;

	@Column(name = "DESCRIPTION")
	private String description;
	
	@Column(name = "BA_CODE")
	private String baCode;
	
	@Column(name = "TAX_CODE")
	private String taxCode;

	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;
	
	@JsonBackReference
	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "CLAIM_INVOICE_DETAILS_LOG_FK1"), name = "CLAIM_INVOICE_LOG_ID", referencedColumnName = "CLAIM_INVOICE_LOG_ID", insertable = false, updatable = false)
	private ClaimInvoiceLog claimInvoiceLog;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "CLAIM_INVOICE_DETAILS_LOG_FK2"), name = "GL_ACCOUNT_CODE", referencedColumnName = "GL_ACCOUNT_CODE", insertable = false, updatable = false)
	private ClaimGlAccount claimGlAccount;

	@Transient
	private ClaimOutputGstTaxCode claimOutputGstTaxCode;

	public Integer getInvoiceDetailId() {
		return invoiceDetailId;
	}

	public void setInvoiceDetailId(Integer invoiceDetailId) {
		this.invoiceDetailId = invoiceDetailId;
	}

	public Integer getInvoiceId() {
		return invoiceId;
	}

	public void setInvoiceId(Integer invoiceId) {
		this.invoiceId = invoiceId;
	}

	public Integer getClaimId() {
		return claimId;
	}

	public void setClaimId(Integer claimId) {
		this.claimId = claimId;
	}

	public String getGlAccountCode() {
		return glAccountCode;
	}

	public void setGlAccountCode(String glAccountCode) {
		this.glAccountCode = glAccountCode;
	}

	public BigDecimal getClaimAmount() {
		return claimAmount;
	}

	public void setClaimAmount(BigDecimal claimAmount) {
		this.claimAmount = claimAmount;
	}

	public BigDecimal getSubContractAmount() {
		return subContractAmount;
	}

	public void setSubContractAmount(BigDecimal subContractAmount) {
		this.subContractAmount = subContractAmount;
	}

	public String getGrtWbs() {
		return grtWbs;
	}

	public void setGrtWbs(String grtWbs) {
		this.grtWbs = grtWbs;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getBaCode() {
		return baCode;
	}

	public void setBaCode(String baCode) {
		this.baCode = baCode;
	}

	public String getTaxCode() {
		return taxCode;
	}

	public void setTaxCode(String taxCode) {
		this.taxCode = taxCode;
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

	public ClaimOutputGstTaxCode getClaimOutputGstTaxCode() {
		return claimOutputGstTaxCode;
	}

	public void setClaimOutputGstTaxCode(ClaimOutputGstTaxCode claimOutputGstTaxCode) {
		this.claimOutputGstTaxCode = claimOutputGstTaxCode;
	}

	public ClaimGlAccount getClaimGlAccount() {
		return claimGlAccount;
	}

	public void setClaimGlAccount(ClaimGlAccount claimGlAccount) {
		this.claimGlAccount = claimGlAccount;
	}

	public ClaimInvoiceLog getClaimInvoiceLog() {
		return claimInvoiceLog;
	}

	public void setClaimInvoiceLog(ClaimInvoiceLog claimInvoiceLog) {
		this.claimInvoiceLog = claimInvoiceLog;
	}

	public Integer getClaimInvoiceLogId() {
		return claimInvoiceLogId;
	}

	public void setClaimInvoiceLogId(Integer claimInvoiceLogId) {
		this.claimInvoiceLogId = claimInvoiceLogId;
	}

}
