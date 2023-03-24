package com.polus.fibicomp.ip.pojo;

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

import org.springframework.data.annotation.CreatedBy;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

@Entity
@Table(name = "PROPOSAL_VERSION_HISTORY")
@EntityListeners(AuditingEntityListener.class)
public class InstituteProposalVersionHistory implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "HISTORY_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SEQ_IP_VER_HIST_ID_GNTR")
	@SequenceGenerator(name = "SEQ_IP_VER_HIST_ID_GNTR", sequenceName = "SEQ_IP_VER_HIST_ID_GNTR", allocationSize = 1)
	private Integer historyId;

	@Column(name = "ACTIVE_PROPOSAL_ID")
	private Integer activeProposalId;

	@Column(name = "ORIGINATED_PROPOSAL_ID")
	private Integer originatedProposalId;

	@Column(name = "REQUEST_TYPE")
	private String requestType;

	@CreatedDate
	@Column(name = "CREATE_TIMESTAMP")
	private Timestamp createTimestamp;

	@CreatedBy
	@Column(name = "CREATE_USER")
	private String createUser;

	public Integer getHistoryId() {
		return historyId;
	}

	public void setHistoryId(Integer historyId) {
		this.historyId = historyId;
	}

	public Integer getActiveProposalId() {
		return activeProposalId;
	}

	public void setActiveProposalId(Integer activeProposalId) {
		this.activeProposalId = activeProposalId;
	}

	public Integer getOriginatedProposalId() {
		return originatedProposalId;
	}

	public void setOriginatedProposalId(Integer originatedProposalId) {
		this.originatedProposalId = originatedProposalId;
	}

	public String getRequestType() {
		return requestType;
	}

	public void setRequestType(String requestType) {
		this.requestType = requestType;
	}

	public Timestamp getCreateTimestamp() {
		return createTimestamp;
	}

	public void setCreateTimestamp(Timestamp createTimestamp) {
		this.createTimestamp = createTimestamp;
	}

	public String getCreateUser() {
		return createUser;
	}

	public void setCreateUser(String createUser) {
		this.createUser = createUser;
	}

}
