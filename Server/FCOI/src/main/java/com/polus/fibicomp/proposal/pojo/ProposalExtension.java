package com.polus.fibicomp.proposal.pojo;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "EPS_PROPOSAL_EXT")
public class ProposalExtension implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "PROPOSAL_ID")
	private Integer proposalId;

	@Column(name = "IS_FOREIGN_ACTIVITIES")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isForeignActivities = false;

	@Column(name = "IS_SUBCONTRACT")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isSubcontract = false;

	@Column(name = "IS_MULTISITE_STUDY")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isMultiSiteStudy = false;

	@Column(name = "IS_DOMESTIC_SITE")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isDomesticSite = false;

	public Integer getProposalId() {
		return proposalId;
	}

	public void setProposalId(Integer proposalId) {
		this.proposalId = proposalId;
	}

	public Boolean getIsForeignActivities() {
		return isForeignActivities;
	}

	public void setIsForeignActivities(Boolean isForeignActivities) {
		this.isForeignActivities = isForeignActivities;
	}

	public Boolean getIsSubcontract() {
		return isSubcontract;
	}

	public void setIsSubcontract(Boolean isSubcontract) {
		this.isSubcontract = isSubcontract;
	}

	public Boolean getIsMultiSiteStudy() {
		return isMultiSiteStudy;
	}

	public void setIsMultiSiteStudy(Boolean isMultiSiteStudy) {
		this.isMultiSiteStudy = isMultiSiteStudy;
	}

	public Boolean getIsDomesticSite() {
		return isDomesticSite;
	}

	public void setIsDomesticSite(Boolean isDomesticSite) {
		this.isDomesticSite = isDomesticSite;
	}
	
}
