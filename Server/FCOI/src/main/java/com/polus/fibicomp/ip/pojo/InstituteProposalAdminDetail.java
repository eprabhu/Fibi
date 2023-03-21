package com.polus.fibicomp.ip.pojo;

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

import org.springframework.data.annotation.CreatedBy;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import com.polus.fibicomp.proposal.pojo.Proposal;

@Entity
@Table(name = "PROPOSAL_ADMIN_DETAILS")
@EntityListeners(AuditingEntityListener.class)
public class InstituteProposalAdminDetail {
	
	@Id
	@Column(name = "PROPOSAL_ADMIN_DETAIL_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SEQ_PROP_ADMIN_DTLS_GNTR")
	@SequenceGenerator(name="SEQ_PROP_ADMIN_DTLS_GNTR", sequenceName = "SEQ_PROP_ADMIN_DTLS_GNTR", allocationSize=1)
	private Integer proposalAdminDetailId;
	
	@Column(name = "DEV_PROPOSAL_ID")
	private Integer devProposalId;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "PROPOSAL_ADMIN_DETAILS_FK1"), name = "DEV_PROPOSAL_ID", referencedColumnName = "PROPOSAL_ID", insertable = false, updatable = false) 
	private Proposal devProposal;

	@Column( name = "INST_PROPOSAL_ID")
	private Integer instProposalId;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "PROPOSAL_ADMIN_DETAILS_FK2"), name = "INST_PROPOSAL_ID", referencedColumnName = "PROPOSAL_ID", insertable = false, updatable = false) 
	private InstituteProposal instituteProposal;

	@Column(name = "DATE_SUBMITTED_BY_DEPT")
	private Timestamp dateSubmittedDept;

	@Column(name = "DATE_RETURNED_TO_DEPT")
	private Timestamp dateReturnedDept;

	@Column(name = "DATE_APPROVED_BY_OSP")
	private Timestamp dateApprovedOsp;

	@Column(name = "DATE_SUBMITTED_TO_AGENCY")
	private Timestamp dateSubmittedAgency;

	@Column(name = "INST_PROP_CREATE_DATE")
	private Timestamp instPropCreateDate;
	
	@Column(name = "INST_PROP_CREATE_USER")
	private String instPropCreateUser;

	@Column(name = "SIGNED_BY")
	private String signedBy;

	@CreatedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@CreatedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Integer getProposalAdminDetailId() {
		return proposalAdminDetailId;
	}

	public void setProposalAdminDetailId(Integer proposalAdminDetailId) {
		this.proposalAdminDetailId = proposalAdminDetailId;
	}

	public Integer getDevProposalId() {
		return devProposalId;
	}

	public void setDevProposalId(Integer devProposalId) {
		this.devProposalId = devProposalId;
	}

	public Integer getInstProposalId() {
		return instProposalId;
	}

	public void setInstProposalId(Integer instProposalId) {
		this.instProposalId = instProposalId;
	}

	public Timestamp getDateSubmittedDept() {
		return dateSubmittedDept;
	}

	public void setDateSubmittedDept(Timestamp dateSubmittedDept) {
		this.dateSubmittedDept = dateSubmittedDept;
	}

	public Timestamp getDateReturnedDept() {
		return dateReturnedDept;
	}

	public void setDateReturnedDept(Timestamp dateReturnedDept) {
		this.dateReturnedDept = dateReturnedDept;
	}

	public Timestamp getDateApprovedOsp() {
		return dateApprovedOsp;
	}

	public void setDateApprovedOsp(Timestamp dateApprovedOsp) {
		this.dateApprovedOsp = dateApprovedOsp;
	}

	public Timestamp getDateSubmittedAgency() {
		return dateSubmittedAgency;
	}

	public void setDateSubmittedAgency(Timestamp dateSubmittedAgency) {
		this.dateSubmittedAgency = dateSubmittedAgency;
	}

	public Timestamp getInstPropCreateDate() {
		return instPropCreateDate;
	}

	public void setInstPropCreateDate(Timestamp instPropCreateDate) {
		this.instPropCreateDate = instPropCreateDate;
	}

	public String getInstPropCreateUser() {
		return instPropCreateUser;
	}

	public void setInstPropCreateUser(String instPropCreateUser) {
		this.instPropCreateUser = instPropCreateUser;
	}

	public String getSignedBy() {
		return signedBy;
	}

	public void setSignedBy(String signedBy) {
		this.signedBy = signedBy;
	}

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

}
