package com.polus.fibicomp.proposal.pojo;

import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

@Entity
@Table(name = "EPS_PROPOSAL_IRB_PROTOCOL")
public class ProposalIrbProtocol implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "IRB_PROTOCOL_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "EPS_PROP_IRB_PROTOCOL_ID_GENERATOR")
	@SequenceGenerator(name = "EPS_PROP_IRB_PROTOCOL_ID_GENERATOR", sequenceName = "EPS_PROP_IRB_PROTOCOL_ID_GENERATOR", allocationSize = 1)
	private Integer irbProtocolId;

	@Column(name = "PROPOSAL_ID")
	private Integer proposalId;

	@Column(name = "PROTOCOL_ID")
	private BigDecimal protocolId;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

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

	public Integer getIrbProtocolId() {
		return irbProtocolId;
	}

	public void setIrbProtocolId(Integer irbProtocolId) {
		this.irbProtocolId = irbProtocolId;
	}

	public BigDecimal getProtocolId() {
		return protocolId;
	}

	public void setProtocolId(BigDecimal protocolId) {
		this.protocolId = protocolId;
	}

	public Integer getProposalId() {
		return proposalId;
	}

	public void setProposalId(Integer proposalId) {
		this.proposalId = proposalId;
	}

}
