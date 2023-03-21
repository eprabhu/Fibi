package com.polus.fibicomp.claims.claimsIntegration.sapfeed.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

@Entity
@Table(name = "SAP_CLAIM_FEED_RESPONSE_MESSGE")
@EntityListeners(AuditingEntityListener.class)
public class SapClaimFeedResponseMessage implements Serializable {

	private static final long serialVersionUID = 1L;
	
	@Id
	@Column(name = "RESPONSE_MESSGE_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SAP_CLAIM_RESP_MES_GENERATOR")
	@SequenceGenerator(name = "SAP_CLAIM_RESP_MES_GENERATOR", sequenceName = "SAP_CLAIM_RESP_MES_GENERATOR", allocationSize = 1)
	private Integer responseMessgeId;

	@Column(name = "CLAIM_INVOICE_LOG_ID")
	private Integer claimInvoiceLogId;
	
	@Column(name = "MESSGAE")
	private String message;
	
	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Integer getResponseMessgeId() {
		return responseMessgeId;
	}

	public void setResponseMessgeId(Integer responseMessgeId) {
		this.responseMessgeId = responseMessgeId;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
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

	public Integer getClaimInvoiceLogId() {
		return claimInvoiceLogId;
	}

	public void setClaimInvoiceLogId(Integer claimInvoiceLogId) {
		this.claimInvoiceLogId = claimInvoiceLogId;
	}

}
